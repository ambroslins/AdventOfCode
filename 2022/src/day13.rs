use ::std::str::FromStr;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending},
    combinator::{all_consuming, map, map_res},
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair},
    IResult,
};

#[derive(Debug, PartialEq, Eq)]
enum Packet {
    Integer(usize),
    List(Vec<Packet>),
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Packet::Integer(x), Packet::Integer(y)) => x.cmp(y),
            (Packet::Integer(x), Packet::List(ys)) => vec![Packet::Integer(*x)].cmp(ys),
            (Packet::List(xs), Packet::Integer(y)) => xs.cmp(&vec![Packet::Integer(*y)]),
            (Packet::List(xs), Packet::List(ys)) => xs.cmp(ys),
        }
    }
}

fn parse_packet(input: &str) -> IResult<&str, Packet> {
    let list = delimited(
        tag("["),
        map(separated_list0(tag(","), parse_packet), Packet::List),
        tag("]"),
    );
    let integer = map(map_res(digit1, usize::from_str), Packet::Integer);
    alt((list, integer))(input)
}

fn parse_input(input: &str) -> IResult<&str, Vec<(Packet, Packet)>> {
    let packet_pair = separated_pair(parse_packet, line_ending, parse_packet);
    separated_list0(pair(line_ending, line_ending), packet_pair)(input)
}

pub fn solve(input: &str) -> (String, String) {
    let (_, packets) = all_consuming(parse_input)(input).unwrap();
    let solution_a: usize = packets
        .iter()
        .enumerate()
        .filter_map(|(i, (first, second))| if first < second { Some(i + 1) } else { None })
        .sum();
    let mut flat_packets: Vec<&Packet> = packets
        .iter()
        .flat_map(|(first, second)| [first, second])
        .collect();
    flat_packets.sort();
    let divider1 = Packet::List(vec![Packet::List(vec![Packet::Integer(2)])]);
    let divider2 = Packet::List(vec![Packet::List(vec![Packet::Integer(6)])]);
    let index1 = flat_packets.partition_point(|&packet| packet < &divider1);
    let index2 = flat_packets.partition_point(|&packet| packet < &divider2);
    let solution_b = (index1 + 1) * (index2 + 2);
    (solution_a.to_string(), solution_b.to_string())
}
