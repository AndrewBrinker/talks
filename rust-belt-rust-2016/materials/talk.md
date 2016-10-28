# Rust Belt Rust

A talk about documentation in Rust, looking at:

- What is the state of documentation in the Rust community?
- What is good, what is bad?
- What can crate maintainers do to improve things?
- What can the Rust team do to improve things?
- How can we make writing docs a more encouraged and encouraging process?

## Outline

### Introduction

Hello and welcome, my name is Andrew Brinker and I'm here today to talk about
documentation in the Rust ecosystem: where it stands, what we can do to
make it better.

First things first: what is documentation, and why should we care about it?
This may seem like a question with an obvious answer, but let's get into it.
Why should we write documentation?

1. Documentation is how you say hello. You may not think about it as such, but
   your project's README _is_ documentation, and it is usually the first thing
   people are going to see. If your project has a website, the copy on that
   website is documentation too. It introduces what the project is, why people
   should be interested, and ideally a bit of how to use it.
2. Documentation gets people to use your project. Most developers are
   risk-averse. There is only a finite amount of time and effort they'll put
   into answering the key question they have about a library. Like stocks, the
   type and amount of risk developers are willing to accept will vary, and the
   more a project's documentation covers, the more likely it is to fit within
   the risk budget of a potential user's project.
3. Documentation gets you new contributors. The less documentation a project
   has, the less likely it is to find contributors who feel sufficiently
   comfortable with it to contribute.
4. Documentation helps keep you sane. It reminds you what's where, and why
   things are done the way they are. It provides you with something to point to
   when people ask questions. If provides a record of the thoughts had
   throughout a project's history.

## Documentation Portfolio

- README
- CONTRIBUTING
- LICENSE
- The Black Triangle (the most basic introduction, gets you doing the basics)
- The Walkthrough (a handwritten tutorial that guides the user through the library,
  from the basic to the advanced)
- The Reference (API docs)
- Issues (open docs-related issues, to which users are directed if they want to make
  or suggest changes or improvements. They should probably come with a promise of
  mentorship through the process)

Optionally:

- A FAQ
- The Expert (method to contact the maintainers / experts in use of the library)

## Outline

- Rust FAQ
  - Occured in three phases
    - Community input: sought out suggestions from the community. What issues do
      people face. What are the common problems?
    - First draft: an initial crack at answering all the questions collected from
      the community
    - Refinement: By far the hardest and longest part. Got lots of feedback from
      the Rust teams and the community. Did tons of drafts of many of the questions.
  - In the end, 3 months, 416 comments, 42 commits
  - Confirmed for me just how hard writing high-quality documentation is. It requires
    time, attention, and a wide array of contributors.
  - Most importantly: docs are never done, and some docs are better than no docs.
  - Immediately after the initial publication, there were a number of additions and
    fixes still to be made. Things are still being updated and refined.
- Docs in the Rust community
  - Rust takes documentation seriously
  - `rustdoc` ships with the compiler, and integrates directly with `cargo`.
  - Looking at the top crates
    - Some crates don't link to their docs at all!
    - Many crates have missing or incomplete documentation
- How do we fix this?
  - Improve documentation for how to write good documentation!
    - I'm working on a guide to maintaining a quality Rust crate, which includes
      lengthy discussion of documentation.
  - Make documentation a stronger focus of existing projects
    - Provide mentored documentation issues for new contributors
    - Require documentation for all builds
- What can Rust do better?
  - Improve Rustdoc
    - There are currently 168 open Rustdoc issues
- Tie into how docs affect productivity (the major next push for Rust)

Overall, the state of documentation in the Rust ecosystem is pretty good, but it
can be better.

TODO:

- Get statistics on % of crates that provide a link to documentation in their
  crates.io page.
- Get statistics on % of mentored tickets in Rustc and Iron that are focused on
  documentation (open vs. closed as well)
- Get statistics on % of total tickets in Rustc and Iron that are focused on
  documentation (open vs. closed as well)
- Get statistics on % of crates that provide documentation links in their
  repository's README

- Talk to the Cargo team about making collecting some of these statistics easier
  - Having a picture of what sort of information the community is providing is
    really useful, including:
    - Docs
    - License
    - Description
    - % 1.0-or-beyond vs. % pre-1.0
    - # of yanked crates and crate versions
    - popularity of keywords
    - popularity of source code hosts (GitHub, Gitlab, Bitbucket, etc.)

Phases of Documentation:

- The pitch: a short, clear introduction to _what the project is_, including some
  example of use and a clear motivating purpose.
- The tutorial: a guided tour to introduce the mechanics and core concepts of the
  library, after which a user should be able to competently make use of the library,
  supplemented by...
- The API docs: complete documentation of every function, type, trait, macro, and
  constant the project offers, including how the parts work together, any caveats
  for use, or _anything else_ that may be necessary for the effective use of the
  project's code.

Talk pitch:

Andrew Brinker talks Rust documentation, including an overview of the documentation
process, the types of documentation that projects should seek to include, where Rust
stands for documentation as a language and a community, and what can be done to make
documentation in Rust a better experience for everyone.

