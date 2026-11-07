import type { PublishedHeaderConsumption } from "midgard-watcher/tests/support/published-da-target-consumption";

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const isHexHash = (value: unknown): value is string =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);

const isIndex = (value: unknown): value is number =>
  typeof value === "number" && Number.isSafeInteger(value) && value >= 0;

/**
 * Every indexed spend of an output that carried the unit, oldest first. Kupo
 * matches are a hint only: the caller authenticates both the creating and the
 * spending transaction through the native recorder before acting on them.
 */
export const readKupoUnitConsumptions = async (
  kupoUrl: string,
  unit: string,
  fetchImpl: typeof fetch = fetch,
): Promise<PublishedHeaderConsumption[]> => {
  const policyId = unit.slice(0, 56);
  const assetName = unit.slice(56);
  if (!/^[0-9a-f]{56}$/u.test(policyId) || !/^[0-9a-f]*$/u.test(assetName))
    throw new Error(`Malformed unit ${unit}`);
  const response = await fetchImpl(
    `${kupoUrl}/matches/${policyId}.${assetName}?spent`,
  );
  if (!response.ok)
    throw new Error(
      `Kupo spent matches for ${unit} failed with ${response.status.toString()}`,
    );
  const matches: unknown = await response.json();
  if (!Array.isArray(matches))
    throw new Error(`Kupo spent matches for ${unit} are not a list`);
  return matches
    .map((match): PublishedHeaderConsumption => {
      const spentAt = isRecord(match) ? match["spent_at"] : undefined;
      if (
        !isRecord(match) ||
        !isHexHash(match["transaction_id"]) ||
        !isIndex(match["output_index"]) ||
        !isRecord(spentAt) ||
        !isHexHash(spentAt["transaction_id"]) ||
        !isIndex(spentAt["slot_no"])
      )
        throw new Error(
          `Kupo spent match for ${unit} lacks an authenticated spend reference`,
        );
      return {
        txHash: match["transaction_id"],
        outputIndex: match["output_index"],
        spentByTxHash: spentAt["transaction_id"],
        spentAtSlot: spentAt["slot_no"],
      };
    })
    .sort((a, b) => a.spentAtSlot - b.spentAtSlot);
};
