# :snowflake: :christmas_tree: :snowman: Advent of Code 2024 :snowman: :christmas_tree: :snowflake:

## About This Repo

Documenting my solutions and progress for [Advent of Code
2024](https://adventofcode.com/2023/). This is my third time attempting
the challenge.

In 2022, I got through Day 8 in Python
<img src="images/python.svg" alt="Python symbol" width="25" height="25"/>.
Last year (2023), I got through Day 11 using <img src="images/R_logo.png" alt="R symbol" width="25" height="25"/>, the language with which I am most comfortable. You can see some of my (very messy) work at [my repo for last year](https://github.com/burrowsdt/advent2023).

This year, I’m starting off in
<img src="images/R_logo.png" alt="R symbol" width="25" height="25"/> but
might jump around a bit. I got through long periods of not coding at all - and my skills are pretty limited - so AOC is primarily a way for me to refresh my knowledge, push my limits and try new things, and of course have a bit of fun. I have a very non-competitive, totally fungible goal of hitting day 13 this year.

I'll log my progress in the [Current Status](#current-status) section below. Although I’m mainly focused on making it work and finding a solution,
I’ll sometimes come back once or twice to refactor the more
interesting/challenging puzzles, or to try a different approach with
performance benchmarks. No final answers are explicitly stated in the
code.

Finally, I try to solve independently at first but have no qualms about looking at other solutions if I realize I'm in over my head. I put brief H/Ts to other solutions that help me find my way in the [Caveats, Comments, and H/Ts](#caveats-comments-and-hts) below. I'll probably experiment a bit with LLMs in considering possible solutions; I'll plan to note that here, too.

## Current Status

| Day    |    Completion     |
|--------|:-----------------:|
| Day 1  |   :star: :star:   |
| Day 2  |   :star: :star:   |
| Day 3  |   :star: :star:   |
| Day 4  |   :star: :star:   |


## Caveats, Comments, and H/Ts:
### Day 2 - 2024-12-02
For fun, I played with the `gt` package to create some (very unremarkable) sparklines of the levels for each report. Like so:
<img src="images/day2_sparklines.png" alt="Snapshot of a table including sparklines" height="100" width="200"/>

See [sparklines.R](days/2024-12-02/sparklines.R) and the examples in the folder. 

## Day 4 - 2024-12-04
Oh shit, it's grid time.
This actually wasn't bad, for the first grid. I intentionally hard-coded a few things as I was in a rush, and there are some weird redundancies in part 1 I could clean up. Added a function for grid construction that should be useable in the future. I could do better at turning some of the recurring set-up steps into functions. 