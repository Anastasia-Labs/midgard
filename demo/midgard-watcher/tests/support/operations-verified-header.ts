import type { WatcherOperationsApi } from "../../src/runtime/operations-observability.js";

/**
 * Only fault decisions are journaled; a healthy classification shows as a
 * verified header in the operations verification diagnostics.
 */
export const operationsVerifiedHeader = (
  api: WatcherOperationsApi,
  headerHash: string,
): boolean => {
  let cursor: string | undefined;
  for (;;) {
    const page = api.diagnostics({
      kind: "verification",
      limit: 100,
      ...(cursor === undefined ? {} : { cursor }),
    });
    if (
      page.records.some(
        (record) =>
          record.kind === "verification" &&
          record.headerHash === headerHash &&
          record.outcome === "verified",
      )
    )
      return true;
    if (page.nextCursor === null) return false;
    cursor = page.nextCursor;
  }
};
