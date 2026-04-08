/**
 * Runtime bridge for the sibling midgard-sdk package.
 * Bundled worker and CLI builds resolve SDK imports through this file so the
 * runtime surface stays pinned to the sibling SDK's built ESM output.
 */
export * from "../../../midgard-sdk/dist/index.js";
