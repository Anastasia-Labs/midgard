/**
 * `@al-ft/midgard-sdk`: every layer of the SDK. Workspace code imports the
 * layer it needs (`@al-ft/midgard-sdk/core`, `/state-queue`, `/da`,
 * `/operators`, `/users`, `/deployment`, `/fraud-proof`); each layer imports
 * only the layers below it, and this root re-exports all of them.
 *
 * The layers are listed lowest first, one per comment so the export sort
 * keeps that order. Each name is exported by one layer only, the one owning
 * its module (`core` carries the fault-proof contract-identity modules, for
 * example), so a test stubs a name on that layer and sees the stub through
 * this root as well.
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
