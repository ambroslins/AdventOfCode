use std::{num::ParseIntError, str::FromStr};

struct Section {
    start: u64,
    end: u64,
}

impl FromStr for Section {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s
            .split_once('-')
            .ok_or_else(|| "missing delimiter -".to_string())?;
        let start = a.parse().map_err(|err: ParseIntError| err.to_string())?;
        let end = b.parse().map_err(|err: ParseIntError| err.to_string())?;
        Ok(Section { start, end })
    }
}

impl Section {
    fn contains(&self, other: &Section) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    fn overlaps(&self, other: &Section) -> bool {
        if self.start <= other.start {
            self.end >= other.start
        } else {
            other.end >= self.start
        }
    }
}

type SectionPair = (Section, Section);

fn parse_section_pair(s: &str) -> Result<SectionPair, String> {
    let (a, b) = s
        .split_once(',')
        .ok_or_else(|| "missing delimiter ,".to_string())?;
    let first = a.parse()?;
    let second = b.parse()?;
    Ok((first, second))
}
pub fn solve(input: &str) -> (String, String) {
    let pairs: Vec<SectionPair> = input.lines().flat_map(parse_section_pair).collect();
    let solution_a = pairs
        .iter()
        .filter(|(first, second)| first.contains(second) || second.contains(first))
        .count();
    let solution_b = pairs
        .iter()
        .filter(|(first, second)| first.overlaps(second))
        .count();
    (solution_a.to_string(), solution_b.to_string())
}
