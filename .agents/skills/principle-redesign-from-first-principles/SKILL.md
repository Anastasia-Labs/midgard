---
name: principle-redesign-from-first-principles
description: "Apply when integrating a new requirement into an existing design. Redesign as if the requirement had been a foundational assumption from day one, instead of bolting it on."
disable-model-invocation: true
---

# Redesign From First Principles

When integrating a change, don't bolt it onto the existing design. Redesign as if the requirement had been there from the start. [review]

- Read all affected files and understand the current design [review]
- Ask: "if we were writing this from scratch with this new requirement, what would we build?" [review]
- Propagate the change through every reference: types, docs, examples, rationale sections [review]
- Think about the whole redesign, then deliver it incrementally [review]

This is the method for preserving option value when integrating changes into an existing design.
