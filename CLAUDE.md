# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Installation: `npm run init`
- Build: `npm run build`
- Start dev server: `npm run serve`
- Watch for changes: `npm run watch`
- Format code: `npm run format`
- Check formatting: `npm run format-check`

## Test Commands
- Run all tests: `npm run test`
- Run tests in watch mode: `npm run test-watch`
- Run a single test: `npx jest src/theory/__tests__/Theory_test.ml -t "specific test name"`

## Code Style Guidelines
- Use OCaml syntax in `.ml` files
- Follow existing formatting conventions (use `npm run format-check` to validate)
- Use Belt for standard library functions (e.g., `open Belt` at the top of files)
- Error handling: Use `Result.Ok`/`Result.Error` and monadic operators (`let+`, `let*`)
- Naming: 
  - Modules use PascalCase
  - Functions/values use snake_case
  - Types use t for the main type in a module
- Imports: Place `open` statements at the top of files or locally in modules
- Types: Define types clearly and use modules to organize related functionality