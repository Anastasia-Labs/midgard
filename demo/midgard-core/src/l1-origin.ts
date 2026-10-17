/**
 * The deployment's L1 origin point O (l1-architecture-plan §5.3): the point
 * immediately before the block that holds the first deployment step,
 * `prepareHubOracleNonce`. The L1 follower starts its replay at O, so every
 * Midgard output lies after it.
 *
 * Until the single redeploy adds `l1Origin` to the finalized manifest, a
 * running deployment supplies O through operator config (node and committee
 * env `L1_ORIGIN`, watcher `l1.origin`), in the text form
 * `<slot>.<block hash>` that `follower find-origin` prints.
 */

/** A chain point: a block's absolute slot and its 32-byte header hash, lowercase hex. */
export type L1Origin = Readonly<{ slot: number; blockHash: string }>;

export class L1OriginFormatError extends Error {
  override readonly name = "L1OriginFormatError";
}

const SLOT_PATTERN = /^(?:0|[1-9][0-9]*)$/u;
const BLOCK_HASH_PATTERN = /^[0-9a-f]{64}$/u;

const requireSlot = (slot: unknown, field: string): number => {
  if (typeof slot !== "number" || !Number.isSafeInteger(slot) || slot < 0)
    throw new L1OriginFormatError(
      `${field}: the slot must be a non-negative safe integer`,
    );
  return slot;
};

const requireBlockHash = (hash: unknown, field: string): string => {
  if (typeof hash !== "string" || !BLOCK_HASH_PATTERN.test(hash))
    throw new L1OriginFormatError(
      `${field}: the block hash must be 64 lowercase hex characters`,
    );
  return hash;
};

/** Parses `<slot>.<64 lowercase hex block hash>`; `field` names the config key in errors. */
export const parseL1Origin = (text: string, field = "l1Origin"): L1Origin => {
  const parts = text.split(".");
  if (parts.length !== 2)
    throw new L1OriginFormatError(
      `${field}: expected <slot>.<block hash>, got ${JSON.stringify(text)}`,
    );
  const [slotText, hash] = parts as [string, string];
  if (!SLOT_PATTERN.test(slotText))
    throw new L1OriginFormatError(
      `${field}: the slot must be a decimal integer without leading zeros`,
    );
  return {
    slot: requireSlot(Number(slotText), field),
    blockHash: requireBlockHash(hash, field),
  };
};

/** Validates an `{slot, blockHash}` record (exactly those two keys). */
export const parseL1OriginRecord = (
  value: unknown,
  field = "l1Origin",
): L1Origin => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new L1OriginFormatError(`${field}: expected {slot, blockHash}`);
  const keys = Object.keys(value).sort();
  if (keys.length !== 2 || keys[0] !== "blockHash" || keys[1] !== "slot")
    throw new L1OriginFormatError(
      `${field}: expected exactly the keys blockHash and slot`,
    );
  const record = value as Readonly<Record<"slot" | "blockHash", unknown>>;
  return {
    slot: requireSlot(record.slot, `${field}.slot`),
    blockHash: requireBlockHash(record.blockHash, `${field}.blockHash`),
  };
};

/** The text form `parseL1Origin` reads. */
export const formatL1Origin = (origin: L1Origin): string =>
  `${origin.slot}.${origin.blockHash}`;

export type L1OriginOrderCheck =
  | Readonly<{ ok: true }>
  | Readonly<{ ok: false; reason: string }>;

/**
 * The origin invariant: O lies strictly before the block that holds the
 * `prepareHubOracleNonce` tx. An origin at or after that block would start
 * the replay after the deployment's first step, so the follower would miss
 * the protocol's creation (R3 at runtime).
 */
export const checkL1OriginBeforeHubOracleNonceBlock = (
  origin: L1Origin,
  nonceBlock: L1Origin,
): L1OriginOrderCheck =>
  origin.slot < nonceBlock.slot
    ? { ok: true }
    : {
        ok: false,
        reason: `l1Origin ${formatL1Origin(origin)} does not lie before the prepareHubOracleNonce block ${formatL1Origin(nonceBlock)}`,
      };
