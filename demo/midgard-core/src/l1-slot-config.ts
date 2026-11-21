/**
 * The deployment's L1 slot-to-time mapping. A committed Midgard body carries
 * its validity interval in L1 slots; a Plutus script sees it as Cardano does,
 * in POSIX milliseconds. The mapping is fixed at deploy time: it is a
 * parameter of the validator that builds the transaction info on chain and is
 * recorded in the deployment manifest, and every role that builds script
 * contexts off chain must run with the same one.
 *
 * The fields are those of Lucid's `SlotConfig`: the Shelley-era anchor slot
 * `zeroSlot`, its POSIX time `zeroTime` in milliseconds, and the slot length in
 * milliseconds. Mainnet, Preprod and Preview use Lucid's static values; a
 * Custom network (the devnet) anchors slot 0 at its Shelley genesis
 * `systemStart`.
 */

export type L1SlotConfig = Readonly<{
  zeroTime: number;
  zeroSlot: number;
  slotLength: number;
}>;

/**
 * Lucid's static slot mappings (`SLOT_CONFIG_NETWORK`) for the named networks:
 * each one's Shelley-era anchor. A Custom network has none; its mapping comes
 * from its Shelley genesis.
 */
export const NAMED_NETWORK_L1_SLOT_CONFIGS = Object.freeze({
  Mainnet: Object.freeze({
    zeroTime: 1596059091000,
    zeroSlot: 4492800,
    slotLength: 1000,
  }),
  Preprod: Object.freeze({
    zeroTime: 1655769600000,
    zeroSlot: 86400,
    slotLength: 1000,
  }),
  Preview: Object.freeze({
    zeroTime: 1666656000000,
    zeroSlot: 0,
    slotLength: 1000,
  }),
} as const satisfies Record<string, L1SlotConfig>);

export class L1SlotConfigFormatError extends Error {
  override readonly name = "L1SlotConfigFormatError";
}

const requireNonNegativeSafeInteger = (
  value: unknown,
  field: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new L1SlotConfigFormatError(
      `${field}: must be a non-negative safe integer`,
    );
  return value;
};

/** Parses an `{ zeroTime, zeroSlot, slotLength }` record with exactly those keys. */
export const parseL1SlotConfig = (
  value: unknown,
  field = "l1SlotConfig",
): L1SlotConfig => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new L1SlotConfigFormatError(`${field}: must be an object`);
  const record = value as Record<string, unknown>;
  const keys = Object.keys(record).sort();
  if (keys.join(",") !== "slotLength,zeroSlot,zeroTime")
    throw new L1SlotConfigFormatError(
      `${field}: must have exactly the keys zeroTime, zeroSlot, slotLength`,
    );
  const slotLength = requireNonNegativeSafeInteger(
    record.slotLength,
    `${field}.slotLength`,
  );
  if (slotLength === 0)
    throw new L1SlotConfigFormatError(`${field}.slotLength: must be positive`);
  return {
    zeroTime: requireNonNegativeSafeInteger(
      record.zeroTime,
      `${field}.zeroTime`,
    ),
    zeroSlot: requireNonNegativeSafeInteger(
      record.zeroSlot,
      `${field}.zeroSlot`,
    ),
    slotLength,
  };
};

export const l1SlotConfigsEqual = (
  left: L1SlotConfig,
  right: L1SlotConfig,
): boolean =>
  left.zeroTime === right.zeroTime &&
  left.zeroSlot === right.zeroSlot &&
  left.slotLength === right.slotLength;

export const formatL1SlotConfig = (config: L1SlotConfig): string =>
  `zeroTime=${config.zeroTime.toString()},zeroSlot=${config.zeroSlot.toString()},slotLength=${config.slotLength.toString()}`;

/**
 * POSIX milliseconds of an L1 slot on the affine Shelley anchor,
 * `zeroTime + (slot - zeroSlot) * slotLength`: the ledger's `slotToPOSIXTime`
 * for every slot from the anchor on. There is no Byron segment, so a
 * pre-Shelley slot maps differently from the ledger (see the time-domain note
 * in `docs/spec/midgard-tx.md`). The on-chain twin is
 * `script_context_v1.slot_to_posix_time_v1`.
 */
export const l1SlotToPosixTimeMs = (
  config: L1SlotConfig,
  slot: bigint,
): bigint =>
  BigInt(config.zeroTime) +
  (slot - BigInt(config.zeroSlot)) * BigInt(config.slotLength);

/**
 * Why a role's runtime slot mapping cannot serve a deployment, or `null` when
 * it matches the deployment's (its manifest's, or the one its scripts were
 * derived with). Roles stay unready on a mismatch: the contexts they would
 * build would differ from the ones the deployed validator builds.
 */
export const l1SlotConfigMismatch = (
  deployment: L1SlotConfig,
  runtime: L1SlotConfig,
): string | null =>
  l1SlotConfigsEqual(deployment, runtime)
    ? null
    : `l1_slot_config_mismatch: runtime ${formatL1SlotConfig(runtime)} differs from the deployment's ${formatL1SlotConfig(deployment)}`;
