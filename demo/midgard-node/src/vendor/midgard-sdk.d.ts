/**
 * Type bridge for the sibling midgard-sdk package.
 * TypeScript resolves `@al-ft/midgard-sdk` through this declaration so worker
 * DTS emit uses the locally synced SDK types instead of traversing the linked
 * package's own dependency graph.
 */
export * from "../generated/midgard-sdk-types";
