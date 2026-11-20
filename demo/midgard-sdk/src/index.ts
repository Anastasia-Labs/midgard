/**
 * `@al-ft/midgard-sdk`: every layer of the SDK. Workspace code imports the
 * layer it needs (`@al-ft/midgard-sdk/core`, `/state-queue`, `/da`,
 * `/operators`, `/users`, `/deployment`, `/fraud-proof`); each layer imports
 * only the layers below it, and this root re-exports all of them.
 *
 * The layers are listed lowest first, one per comment so the export sort
 * keeps that order: `core` also carries the fault-proof contract-identity
 * modules that `/fraud-proof` re-exports, and a test that stubs one of those
 * names on `core` must see its stub through this root as well, which the
 * test runner's `export *` gives only to the first layer listing the name.
 */
// Layer 0.
export * from "./layers/core.js";
// Layer 1, over core.
export * from "./layers/state-queue.js";
// Layer 2, over state-queue.
export * from "./layers/da.js";
// Layer 2, over state-queue.
export * from "./layers/operators.js";
// Layer 2, over state-queue.
export * from "./layers/users.js";
// Layer 3, over da.
export * from "./layers/deployment.js";
// Layer 4, over deployment.
export * from "./fraud-proof/index.js";
