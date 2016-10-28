The plan for this talk is to:

- Discuss my experiences with writing and contributing to documentation in the
  Rust ecosystem
- Discuss what constitutes good documentation for a Rust crate, including:
  - Introductory explanatory documentation, often found in the README, which
    describes what the crate is, why it's useful, and how to use it
  - Tutorial documentation, which introduces the crate's major concepts and
    usage, enabling users to use the crate effectively
  - API documentation, which provides a complete reference to all public
    functions, types, traits, constants, and macros in the crate
- Discuss statistics on documentation across all Rust crates, including:
  - How many crates provide a link to documentation on crates.io
  - How many of those links work
  - The quality of documentation for the top crates (complete w/ explanations,
    complete w/ partial explanations, complete w/out explanations, incomplete,
    missing)
  - % of issues related to documentation in top Rust crates (open vs. closed
    as well)
- Discuss ideas for how Rust documentation may be improved, including:
  - Providing mentored documentation issues, encouraging documentation
    contributions as a learning experience for new contributors
  - Improving community documentation standards
  - Making sure to use `#![deny(missing_docs)]`
  - Automatically publishing up-to-date documentation with each new crate
    version

