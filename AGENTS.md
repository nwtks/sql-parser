# AGENTS.md

This file provides guidance for AI agents working in this repository.

## AGENTS.md Editing Rules

- **Don't write what's in the codebase** — information that can be obtained by reading source code or project files must not be written in AGENTS.md.
- **Don't duplicate README.md** — content already described in README.md should only be referenced by a link (`See [README.md](...)`).

### Documentation Location Rules

| Topic | Destination |
|-------|-------------|
| Architecture and design discussions | `docs/architecture.md` |
| Design trade-offs | `docs/trade-off.md` |
| Common mistakes / gotchas | `docs/gotchas.md` |

- **When a design decision, trade-off, bug fix, or known issue occurs, update `docs/trade-off.md` or `docs/gotchas.md` immediately (in the same session) — do not defer.**
- When a new trade-off or gotcha arises, first consider appending to the relevant `docs/` file. Only add to AGENTS.md if it's an "implicit rule not obvious from the codebase."
- Only keep project-specific implicit rules in AGENTS.md. The topics above belong in their corresponding `docs/*.md` files.

---

## Cross-Platform Compatibility

All code — including test code — must work on **both Windows and Linux**. Avoid:

- Hard-coded path separators; use `System.IO.Path.Combine`.
- Platform-specific APIs without fallback.
- Assumptions about case-sensitive file paths.
- Process-level locks on files that outlive the test scope.

---

## Coding Conventions

- **Functional-first** — Prefer functional programming idioms over imperative ones throughout the codebase — including test code. Use recursion, immutability, and composition over loops, mutation, and statements.
- **Favor expressions over statements** — Use `match` expressions, `if`/`then`/`else`, and pattern matching instead of imperative control flow. Every branch should produce a value.
- **Leverage discriminated unions** — Model domain concepts with DUs for exhaustiveness checking.
- **Use `[<TailCall>]` on recursive functions** that loop to prevent stack overflows.
- Prefer immutable data, `Result<'T, string>` for error handling, and pipeline operators (`|>`).
- Do not introduce new external NuGet packages without checking existing dependencies in the `.fsproj` files first.

---

## Testing Conventions

- After any code change, run `dotnet test` and confirm **all tests pass**.
- Maintain high unit test coverage (target: ≥ 90% line coverage). If it falls below 90%, add tests to restore it before merging.
- **Test ordering rules**:
  1. Within each test file, `[<Fact>]` functions must appear in the same order as the corresponding functions/methods/constructors in the source file under test.
  2. When multiple test cases target the same source function, order them by **test priority**: normal (happy path) → error cases → fault/failure scenarios.
- **Prefer data-driven tests** (`[<Theory>]` + `[<InlineData>]`) when multiple test cases share the same test logic but differ only in inputs or expected outputs. This reduces code duplication and makes it easy to add new cases.
- **Use a unique suffix** per test — tests may run in parallel.
