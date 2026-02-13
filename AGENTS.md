# AGENTS.md

Guidance for coding agents and contributors working in this repository. This is a tool for working with the
International Image Interoperability Framework (IIIF).

Specifications for this framework can be found at:

 - Image API v3: https://iiif.io/api/image/3.0/
 - Presentation API v3: https://iiif.io/api/presentation/3.0/
 - Image API v2: https://iiif.io/api/image/2.1/
 - Presentatino API v2: https://iiif.io/api/presentation/2.1/

## Scope

These instructions apply to the entire repository.

## Project Overview

- Language: Elm
- Purpose: IIIF v2/v3 helpers, decoders, and types
- Main source directory: `src/`
- Tests: `tests/`

## Setup

- Install dependencies: `yarn install`
- Run tests: `yarn test`

## Coding Guidelines

- Prefer small, focused changes.
- Keep backwards compatibility for existing v2/v3 decoders unless a breaking change is explicitly requested.
- Add or update tests for behavior changes, especially decoder edge cases.
- Match existing module style and naming.
- After making changes run `elm-format` to ensure the file conforms to the Elm coding guidelines.

## Validation

- Run `yarn test` after making changes.
- If tests cannot be run, explain why in your final update.

## Notes for Decoder Work

- Many decoders normalize v2 and v3 shapes into shared types.
- When adding support for real-world manifest variants, prefer extending decoder `oneOf` branches rather than replacing existing branches.
