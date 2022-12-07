use std::{collections::HashMap, str::FromStr};

#[derive(Debug)]
struct File {
    size: usize,
}

#[derive(Debug)]
struct Dir {
    dirs: HashMap<String, Dir>,
    files: HashMap<String, File>,
}

impl Dir {
    fn new() -> Dir {
        return Dir {
            dirs: HashMap::new(),
            files: HashMap::new(),
        };
    }

    fn size(&self) -> usize {
        return self.dirs.values().map(Dir::size).sum::<usize>()
            + self.files.values().map(|file| file.size).sum::<usize>();
    }
}

#[derive(Debug)]
enum Command {
    Cd(String),
    Ls(Vec<String>),
}

impl FromStr for Command {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (command, rest) = s.split_at(2);
        match command {
            "cd" => Ok(Command::Cd(rest.trim().to_string())),
            "ls" => Ok(Command::Ls(
                rest.trim().lines().map(|line| line.to_string()).collect(),
            )),
            _ => Err(format!("unexpected command: {command}")),
        }
    }
}

fn run_commands(commands: &Vec<Command>) -> Dir {
    let mut root = Dir::new();
    let mut cwd: Vec<String> = Vec::new();
    for command in commands {
        match command {
            Command::Cd(path) => match path.as_str() {
                "/" => cwd.clear(),
                ".." => {
                    cwd.pop();
                }
                _ => cwd.push(path.clone()),
            },
            Command::Ls(lines) => {
                let dir = cwd
                    .iter()
                    .fold(&mut root, |d, path| d.dirs.get_mut(path).unwrap());
                for line in lines {
                    let (size, name) = line.split_once(' ').unwrap();
                    if size == "dir" {
                        dir.dirs.insert(name.to_string(), Dir::new());
                    } else {
                        dir.files.insert(
                            name.to_string(),
                            File {
                                size: size.parse().unwrap(),
                            },
                        );
                    }
                }
            }
        }
    }

    return root;
}

fn flat_sizes(dir: &Dir) -> Vec<usize> {
    return dir
        .dirs
        .values()
        .map(flat_sizes)
        .flatten()
        .chain(std::iter::once(dir.size()))
        .collect();
}

// this is pretty bad, we calculate the size of each directory multiple times
pub fn solve(input: &str) -> (String, String) {
    let commands: Vec<Command> = input
        .split("$")
        .filter(|cmd| !cmd.is_empty())
        .map(|cmd| {
            return cmd.trim().parse::<Command>().unwrap();
        })
        .collect();
    let root = run_commands(&commands);
    let sizes = flat_sizes(&root);
    let solution_a: usize = sizes.iter().filter(|&&size| size <= 100000).sum();
    let available = 70000000;
    let required = 30000000;
    let total = sizes.last().unwrap();
    let unused = available - total;
    let delete = required - unused;
    let solution_b: usize = *sizes.iter().filter(|&&size| size >= delete).min().unwrap();
    return (solution_a.to_string(), solution_b.to_string());
}
