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
    widgets::{self, Axis, Block, Borders, Cell, Dataset, Paragraph},
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
    position_per_group: Vec<(usize, usize)>,
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
            position_per_group: vec![(0, 0); group_count],
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
                "<ctrl-j>".green().bold(),
                " prev arg ".into(),
                "<ctrl-k>".green().bold(),
                " next function ".into(),
                "<ctrl-l>".magenta().bold(),
                " prev function ".into(),
                "<ctrl-h>".magenta().bold(),
                " reload file ".into(),
                "<ctrl-r>".red().bold(),
                " quit ".into(),
                "<q>".red().bold(),
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

            if self.position_per_group[self.group_idx].0 == 0 {
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

                        let data = data
                            .header(widgets::Row::new(header))
                            .widths(std::iter::once(Constraint::Min(14)).chain(
                                std::iter::repeat(Constraint::Min(10)).take(group.function.len()),
                            ))
                            .highlight_style(Style::default().black().on_white());

                        frame.render_stateful_widget(list, layout[0], &mut list_state);
                        frame.render_widget(data.block(bottom_block), layout[1]);
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
                                .enumerate()
                                .map(|(y, row)| {
                                    row.enumerate().map(move |(x, cell)| {
                                        let mut cell = Cell::new(cell);
                                        if (x, y) == self.position_per_group[self.group_idx] {
                                            cell = cell.style(Style::new().black().on_white())
                                        }
                                        cell
                                    })
                                })
                                .map(widgets::Row::new),
                        );

                        let data = data
                            .header(widgets::Row::new(header))
                            .widths(std::iter::once(Constraint::Min(14)).chain(
                                std::iter::repeat(Constraint::Min(10)).take(group.function.len()),
                            ))
                            .highlight_style(Style::default().black().on_white());

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
                                        let (x, y) = (arg.0 as f64, metrics[arg_idx][func_idx].0);
                                        xmin = xmin.min(x);
                                        xmax = xmax.max(x);
                                        ymin = ymin.min(y);
                                        ymax = ymax.max(y);
                                        (x, y)
                                    })
                                    .filter(|(_, y)| !y.is_nan())
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
                        frame.render_widget(data.block(bottom_block), layout[1]);
                    }
                }
            } else {
                let data = match &group.args {
                    BenchArgs::Named(args) => widgets::Table::default()
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
                        .column_spacing(3),
                    BenchArgs::Plot(args) => widgets::Table::default().block(block.clone()).rows(
                        args.iter()
                            .map(|f| format!("{f:?}"))
                            .zip(timings.iter().map(|timings| {
                                timings
                                    .iter()
                                    .map(|(mean, stddev)| format!("{mean:?} ± {stddev:?}"))
                            }))
                            .map(|(name, data)| std::iter::once(name).chain(data))
                            .enumerate()
                            .map(|(y, row)| {
                                row.enumerate().map(move |(x, cell)| {
                                    let mut cell = Cell::new(cell);
                                    if (x, y) == self.position_per_group[self.group_idx] {
                                        cell = cell.style(Style::new().black().on_white())
                                    }
                                    cell
                                })
                            })
                            .map(widgets::Row::new),
                    ),
                };

                let data =
                    data.header(widgets::Row::new(header))
                        .widths(std::iter::once(Constraint::Min(14)).chain(
                            std::iter::repeat(Constraint::Min(10)).take(group.function.len()),
                        ))
                        .highlight_style(Style::default().black().on_white());

                let hist_width = split_layout[1].width.saturating_sub(4) as usize;
                let hist_height = split_layout[1].height.saturating_sub(8) as usize;
                let f_idx = self.position_per_group[self.group_idx].0 - 1;
                let arg_idx = self.position_per_group[self.group_idx].1;
                let mut drew_block = false;
                'stop: {
                    if hist_width > 2 && hist_height > 4 {
                        let histbars = [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"];
                        let mut bins = vec![0usize; hist_width];
                        let (timings, _) = group.at(Func(f_idx), Arg(arg_idx));
                        let mut timings = timings.to_vec();
                        timings.sort_unstable();

                        // remove outliers, taken from julia benchmarktools
                        let mut sum = timings.iter().copied().sum::<Picoseconds>();
                        while timings.len() > 0
                            && sum / timings.len() as i128 > timings[timings.len() / 2]
                        {
                            let removed = timings.pop().unwrap();
                            sum -= removed;
                        }

                        if timings.len() == 0 {
                            break 'stop;
                        }

                        let min_t = timings.iter().min().unwrap().to_secs();
                        let max_t = timings.iter().max().unwrap().to_secs();

                        for &time in &*timings {
                            let time = time.to_secs();
                            let bin_idx =
                                (hist_width as f64 * ((time - min_t) / (max_t - min_t))).max(0.0);
                            let mut bin_idx = bin_idx as usize;
                            if bin_idx >= bins.len() {
                                bin_idx = bins.len() - 1;
                            }
                            bins[bin_idx] += 1;
                        }

                        let mut text = vec![vec![" "; hist_width + 4]; hist_height + 2];
                        let norm = bins.iter().copied().max().unwrap() as f64;
                        for i in 0..hist_height {
                            let y_top = (hist_height - i) as f64 / hist_height as f64;
                            let y_bot = (hist_height - i - 1) as f64 / hist_height as f64;

                            for j in 0..hist_width {
                                let y = bins[j] as f64 / norm;
                                let c = &mut text[i + 2][j + 2];
                                if y >= y_top {
                                    *c = *histbars.last().unwrap();
                                } else if y <= y_bot {
                                    *c = *histbars.first().unwrap();
                                } else {
                                    let t = (y - y_bot) / (y_top - y_bot);
                                    let idx = (t * histbars.len() as f64) as usize;
                                    let idx = idx.clamp(0, histbars.len() - 1);
                                    *c = histbars[idx];
                                }
                            }
                        }

                        let mut text = Text::from(
                            text.into_iter()
                                .map(|line| line.join(""))
                                .collect::<Vec<_>>()
                                .join("\n"),
                        );

                        let line_width = split_layout[1].width as usize - 2;
                        let mut line = vec!["─"; line_width];
                        let mut labels = vec![];
                        let len = format!(" {:?} ", timings[0]).len();
                        let mut total_len = 0;
                        loop {
                            if total_len + len >= line_width {
                                break;
                            }
                            let next = total_len + len / 2;
                            let t = next as f64 / line_width as f64;
                            let next_t = format!(
                                " {:?} ",
                                Picoseconds(((min_t + t * (max_t - min_t)) * 1e12) as i128)
                            );
                            line[total_len + len / 2] = "┬";
                            total_len += next_t.len();
                            labels.push(next_t.into());
                        }
                        text.lines.push(Line::from(line.join("")));
                        text.lines.push(Line::from(labels));
                        drew_block = true;
                        frame.render_widget(
                            Paragraph::new(text).block(block.clone()),
                            split_layout[1],
                        );
                    }
                }
                if !drew_block {
                    frame.render_widget(block.clone(), split_layout[1]);
                }
                frame.render_stateful_widget(list, split_layout[0], &mut list_state);
                frame.render_widget(data.block(bottom_block), layout[1]);
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

            KeyCode::Char('h') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                self.position_per_group[self.group_idx].0 =
                    self.position_per_group[self.group_idx].0.saturating_sub(1);
            }

            KeyCode::Char('l') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                self.position_per_group[self.group_idx].0 = Ord::min(
                    self.result.groups[self.group_idx].function.len(),
                    self.position_per_group[self.group_idx].0 + 1,
                );
            }

            KeyCode::Char('k') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    self.position_per_group[self.group_idx].1 =
                        self.position_per_group[self.group_idx].1.saturating_sub(1);
                } else {
                    self.group_idx = self.group_idx.saturating_sub(1);
                }
            }
            KeyCode::Char('j') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    self.position_per_group[self.group_idx].1 = Ord::min(
                        self.result.groups[self.group_idx]
                            .args
                            .len()
                            .saturating_sub(1),
                        self.position_per_group[self.group_idx].1 + 1,
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
