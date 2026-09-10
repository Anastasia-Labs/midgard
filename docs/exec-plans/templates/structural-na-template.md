# Structural non-applicability evidence template

Use this structure when a proposed standalone family reduces to another
implemented rule or cannot arise in an admitted transaction. This is a template,
not an executed test result. See [decision 0003](../../midgard/decisions/0003-q27-q32-q43-owner-decisions.md)
for required-signer and accepted-transition precedents.

- Coverage row and semantic violation:
- Claim kind: reduces to an existing path, or structurally impossible:
- Normative decision and current source implementing the constraint:
- Existing proof route that covers the violation, if applicable:

## Executable evidence

Identify current test files and selectors. Record the working directory, exact
command, source revision, exit status, and the cases actually collected.
Demonstrate the claimed reduction with a valid control and a case that would
violate the rule. A fixture rejected before the relevant boundary does not prove
the reduction. Where a mutation control is needed, identify the invariant it
exercises and the observed outcome.

## Coverage consequence

Explain why a separate deployed family would duplicate an existing proof or
cannot apply. Link the maintained coverage row and any remaining operational
acceptance. A structural decision does not itself prove the existing route is
complete, reachable, or live-tested.
