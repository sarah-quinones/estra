use clap::{Parser, ValueEnum};
use color_eyre::{config::HookBuilder, eyre};
use crossterm::{
    event::{self, KeyCode, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use diol::{result::*, Picoseconds};
use ratatui::{
    prelude::*,
    widgets::{self, Axis, Block, Borders, Dataset},
};
use std::{
    io::{self, stdout, Stdout},
    panic,
    path::PathBuf,
};

#[derive(ValueEnum, Debug, Clone, Copy)]
#[clap(rename_all = "kebab_case")]
enum Colors {
    CubehelixDefault,
    Turbo,
    Spectral,
    Viridis,
    Magma,
    Inferno,
    Plasma,
    Cividis,
    Warm,
    Cool,
}

#[derive(Debug)]
struct App {
    path: PathBuf,
    colors: Colors,
    result: BenchResult,
    group_idx: usize,
    arg_idx_per_group: Vec<usize>,
    exit: bool,
}

type Tui = Terminal<CrosstermBackend<Stdout>>;

impl App {
    fn new(path: PathBuf, colors: Colors) -> io::Result<Self> {
        let file = std::fs::File::open(&path)?;
        let result: BenchResult = serde_json::from_reader(io::BufReader::new(file))?;
        let group_count = result.groups.len();
        let app = App {
            path,
            colors,
            result,
            group_idx: 0,
            arg_idx_per_group: vec![0; group_count],
            exit: false,
        };
        Ok(app)
    }

    /// runs the application's main loop until the user quits
    fn run(&mut self, terminal: &mut Tui) -> io::Result<()> {
        while !self.exit {
            terminal.draw(|frame| self.render_frame(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn render_frame(&self, frame: &mut Frame) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(frame.size());

        let block = Block::new().borders(Borders::all()).white().on_black();
        let bottom_block = block
            .clone()
            .title_bottom(Line::from(vec![
                " next group ".into(),
                "<j>".blue().bold(),
                " prev group ".into(),
                "<k>".blue().bold(),
                " next arg ".into(),
                "<ctrl-j>".blue().bold(),
                " prev arg ".into(),
                "<ctrl-k>".blue().bold(),
                " reload file ".into(),
                "<ctrl-r>".blue().bold(),
            ]))
            .title_alignment(Alignment::Center);

        let list = widgets::List::new(self.result.groups.iter().map(|group| {
            let suffix = if group.function.len() <= 1 { "" } else { "..." };
            group
                .function
                .iter()
                .next()
                .map(|f| format!("{}{suffix}", &*f.name))
                .unwrap_or(String::new())
        }))
        .highlight_style(Style::default().black().on_white())
        .block(block.clone());
        let mut list_state = widgets::ListState::default();
        if !self.result.groups.is_empty() {
            list_state = list_state.with_selected(Some(self.group_idx))
        }

        if let Some(group) = self.result.groups.get(self.group_idx) {
            let split_layout = Layout::default()
                .direction(Direction::Horizontal)
                .constraints(vec![Constraint::Percentage(25), Constraint::Percentage(75)])
                .split(layout[0]);

            let args_len = match &group.args {
                BenchArgs::Named(args) => args.len(),
                BenchArgs::Plot(args) => args.len(),
            };

            let mut header = vec!["args"];

            let mut timings =
                vec![vec![(Picoseconds(0), Picoseconds(0)); group.function.len()]; args_len];
            let mut metrics = vec![vec![(0.0, 0.0); group.function.len()]; args_len];

            for (func_idx, func) in group.function.iter().enumerate() {
                header.push(&func.name);

                for arg_idx in 0..func.timings.len() {
                    let (time, metric) = func.at(Arg(arg_idx));
                    timings[arg_idx][func_idx] = time.mean_stddev();
                    if let Some(metric) = metric {
                        metrics[arg_idx][func_idx] = metric.mean_stddev();
                    }
                }
            }

            match &group.args {
                BenchArgs::Named(args) => {
                    let data = widgets::Table::default()
                        .block(block.clone())
                        .rows(
                            args.iter()
                                .map(|f| f.clone())
                                .zip(timings.iter().map(|timings| {
                                    timings
                                        .iter()
                                        .map(|(mean, stddev)| format!("{mean:?} ± {stddev:?}"))
                                }))
                                .map(|(name, data)| std::iter::once(name).chain(data))
                                .map(widgets::Row::new),
                        )
                        .column_spacing(3);
                    let mut data_state = widgets::TableState::new();

                    let data = data
                        .header(widgets::Row::new(header))
                        .widths(std::iter::once(Constraint::Min(14)).chain(
                            std::iter::repeat(Constraint::Min(10)).take(group.function.len()),
                        ))
                        .highlight_style(Style::default().black().on_white());

                    if !args.is_empty() {
                        data_state =
                            data_state.with_selected(Some(self.arg_idx_per_group[self.group_idx]));
                    }

                    frame.render_stateful_widget(list, layout[0], &mut list_state);
                    frame.render_stateful_widget(data, layout[1], &mut data_state);
                }
                BenchArgs::Plot(args) => {
                    let data = widgets::Table::default().block(block.clone()).rows(
                        args.iter()
                            .map(|f| format!("{f:?}"))
                            .zip(timings.iter().map(|timings| {
                                timings
                                    .iter()
                                    .map(|(mean, stddev)| format!("{mean:?} ± {stddev:?}"))
                            }))
                            .map(|(name, data)| std::iter::once(name).chain(data))
                            .map(widgets::Row::new),
                    );
                    let mut data_state = widgets::TableState::new();

                    let data = data
                        .header(widgets::Row::new(header))
                        .widths(std::iter::once(Constraint::Min(14)).chain(
                            std::iter::repeat(Constraint::Min(10)).take(group.function.len()),
                        ))
                        .highlight_style(Style::default().black().on_white());

                    if !args.is_empty() {
                        data_state =
                            data_state.with_selected(Some(self.arg_idx_per_group[self.group_idx]));
                    }

                    let mut chart_data = vec![];

                    let mut xmin = 0.0f64;
                    let mut xmax = 0.0f64;
                    let mut ymin = f64::INFINITY;
                    let mut ymax = f64::NEG_INFINITY;

                    let metric_name = &*group.metric_name;

                    for func_idx in 0..group.function.len() {
                        chart_data.push(
                            args.iter()
                                .enumerate()
                                .map(|(arg_idx, &arg)| {
                                    let (x, y) = (arg.0 as f64, metrics[arg_idx][func_idx].1);
                                    xmin = xmin.min(x);
                                    xmax = xmin.max(x);
                                    ymin = ymin.min(y);
                                    ymax = ymax.max(y);
                                    (x, y)
                                })
                                .collect::<Vec<_>>(),
                        )
                    }

                    ymin = ymin.min(0.0);

                    let mut datasets = vec![];
                    for (idx, (func, chart_data)) in
                        std::iter::zip(&group.function, &chart_data).enumerate()
                    {
                        let colors = match self.colors {
                            Colors::CubehelixDefault => colorgrad::cubehelix_default(),
                            Colors::Turbo => colorgrad::turbo(),
                            Colors::Spectral => colorgrad::spectral(),
                            Colors::Viridis => colorgrad::viridis(),
                            Colors::Magma => colorgrad::magma(),
                            Colors::Inferno => colorgrad::inferno(),
                            Colors::Plasma => colorgrad::plasma(),
                            Colors::Cividis => colorgrad::cividis(),
                            Colors::Warm => colorgrad::warm(),
                            Colors::Cool => colorgrad::cool(),
                        };

                        let color = if group.function.len() <= 1 {
                            colors.at(0.5)
                        } else {
                            colors.at(idx as f64 / (group.function.len() - 1) as f64)
                        };
                        datasets.push(
                            Dataset::default()
                                .name(&*func.name)
                                .graph_type(widgets::GraphType::Line)
                                .marker(Marker::Braille)
                                .data(chart_data)
                                .style(Style::default().fg(Color::Rgb(
                                    (color.r * 255.0) as u8,
                                    (color.g * 255.0) as u8,
                                    (color.b * 255.0) as u8,
                                ))),
                        );
                    }

                    // Create the X axis and define its properties
                    let x_axis = Axis::default()
                        .title("n".red())
                        .style(Style::default().white())
                        .bounds([xmin, xmax])
                        .labels(vec![
                            format!("{xmin}").into(),
                            format!("{}", (xmin + xmax) / 2.0).into(),
                            format!("{xmax}").into(),
                        ]);

                    // Create the Y axis and define its properties
                    let y_axis = Axis::default()
                        .title(metric_name.red())
                        .style(Style::default().white())
                        .bounds([ymin, ymax])
                        .labels(vec![
                            format!("{ymin:.2e}").into(),
                            format!("{:.2e}", (ymin + ymax) / 2.0).into(),
                            format!("{ymax:.2e}").into(),
                        ]);

                    let chart = widgets::Chart::new(datasets)
                        .block(block.clone())
                        .x_axis(x_axis)
                        .y_axis(y_axis)
                        .hidden_legend_constraints((Constraint::Min(0), Constraint::Min(0)))
                        .legend_position(Some(widgets::LegendPosition::TopLeft));

                    frame.render_stateful_widget(list, split_layout[0], &mut list_state);
                    frame.render_widget(chart, split_layout[1]);
                    frame.render_stateful_widget(
                        data.block(bottom_block),
                        layout[1],
                        &mut data_state,
                    );
                }
            }
        } else {
            frame.render_stateful_widget(list, layout[0], &mut list_state);
            frame.render_widget(bottom_block, layout[1]);
        }
    }

    fn handle_events(&mut self) -> io::Result<()> {
        match event::read()? {
            // it's important to check that the event is a key press event as
            // crossterm also emits key release and repeat events on Windows.
            event::Event::Key(key_event) if key_event.kind == event::KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        };
        Ok(())
    }

    fn handle_key_event(&mut self, key_event: event::KeyEvent) {
        if self.result.groups.is_empty() {
            return;
        }

        match key_event.code {
            KeyCode::Char('q') => self.exit(),
            KeyCode::Char('k') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    self.arg_idx_per_group[self.group_idx] =
                        self.arg_idx_per_group[self.group_idx].saturating_sub(1);
                } else {
                    self.group_idx = self.group_idx.saturating_sub(1);
                }
            }
            KeyCode::Char('j') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    self.arg_idx_per_group[self.group_idx] = Ord::min(
                        self.result.groups[self.group_idx]
                            .args
                            .len()
                            .saturating_sub(1),
                        self.arg_idx_per_group[self.group_idx] + 1,
                    );
                } else {
                    self.group_idx = Ord::min(self.result.groups.len() - 1, self.group_idx + 1)
                }
            }
            KeyCode::Char('r') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                if let Ok(new) = Self::new(self.path.clone(), self.colors) {
                    *self = new;
                }
            }
            _ => {}
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }
}

fn main() -> eyre::Result<()> {
    #[derive(Parser)]
    struct Clap {
        /// `diol` json output file path.
        path: PathBuf,
        #[arg(long)]
        colors: Option<Colors>,
    }
    let clap = Clap::parse();

    install_hooks()?;
    let mut tui = init_tui()?;

    let mut app = App::new(clap.path, clap.colors.unwrap_or(Colors::Spectral))?;
    app.run(&mut tui)?;

    restore_tui()?;

    Ok(())
}

fn init_tui() -> io::Result<Tui> {
    enable_raw_mode()?;
    execute!(stdout(), EnterAlternateScreen)?;
    Terminal::new(CrosstermBackend::new(stdout()))
}

fn restore_tui() -> io::Result<()> {
    disable_raw_mode()?;
    execute!(stdout(), LeaveAlternateScreen)?;
    Ok(())
}

/// This replaces the standard color_eyre panic and error hooks with hooks that
/// restore the terminal before printing the panic or error.
fn install_hooks() -> color_eyre::Result<()> {
    let (panic_hook, eyre_hook) = HookBuilder::default().into_hooks();

    // convert from a color_eyre PanicHook to a standard panic hook
    let panic_hook = panic_hook.into_panic_hook();
    panic::set_hook(Box::new(move |panic_info| {
        restore_tui().unwrap();
        panic_hook(panic_info);
    }));

    // convert from a color_eyre EyreHook to a eyre ErrorHook
    let eyre_hook = eyre_hook.into_eyre_hook();
    eyre::set_hook(Box::new(
        move |error: &(dyn std::error::Error + 'static)| {
            restore_tui().unwrap();
            eyre_hook(error)
        },
    ))?;

    Ok(())
}
