import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { DaPayloadRecord } from "../domain.js";
import { hexToBytes } from "../utils/hex.js";
import { validateEntries } from "./payload.da-payload-validation-error.js";
import type { PreBlockUtxos } from "./payload.validate-event-program-coverage.js";
import { computeDaPayloadUtxosRoot } from "./payload.verify-da-payload-against-header.js";
import type { DaPayloadSource } from "./source.js";

/**
 * The state before a block cannot be established: no retained or fetched
 * parent payload carries a UTxO set whose root is the block's
 * `prevUtxosRoot`. The block is not attested until it can be.
 */
export class ParentStateUnavailableError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ParentStateUnavailableError";
  }
}

const utxosFromPayloadBytes = async (
  storedPayloadCbor: Uint8Array,
): Promise<readonly SDK.DaPayloadEntry[]> => {
  const { innerBytes } = await unwrapDaPayload(Buffer.from(storedPayloadCbor), {
    maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
  });
  const utxos = SDK.decodeDaPayload(innerBytes).block_body.utxos;
  validateEntries("parent utxos", utxos);
  return utxos;
};

/** The candidate's UTxO set when its root is `expectedRoot`, else undefined. */
const boundUtxos = async (
  storedPayloadCbor: Uint8Array,
  expectedRoot: string,
): Promise<readonly SDK.DaPayloadEntry[] | undefined> => {
  try {
    const utxos = await utxosFromPayloadBytes(storedPayloadCbor);
    return (await computeDaPayloadUtxosRoot(utxos)) === expectedRoot
      ? utxos
      : undefined;
  } catch {
    return undefined;
  }
};

const toPreBlockUtxos = (utxos: readonly SDK.DaPayloadEntry[]): PreBlockUtxos =>
  utxos.map(
    ([outRefHex, outputHex]) =>
      [outRefHex, hexToBytes(outputHex, "parent utxos value")] as const,
  );

/**
 * Establishes the L2 UTxO set immediately before the block `header`: the
 * empty set before the first block, otherwise the parent block's post-state
 * (`body.utxos` of its retained payload, or of a payload fetched from peers).
 * Whatever its source, the set is accepted only when its root equals
 * `header.prevUtxosRoot`, so a stored record's status is never trusted.
 */
export const resolvePreBlockUtxos = async ({
  header,
  getDaPayload,
  payloadSource,
}: {
  readonly header: SDK.Header;
  readonly getDaPayload: (
    headerHash: string,
  ) => Promise<Pick<DaPayloadRecord, "payloadCborHex"> | undefined>;
  readonly payloadSource: DaPayloadSource;
}): Promise<PreBlockUtxos> => {
  const expectedRoot = header.prevUtxosRoot;
  if (header.prevHeaderHash === SDK.GENESIS_HEADER_HASH) {
    if ((await computeDaPayloadUtxosRoot([])) === expectedRoot) return [];
    throw new ParentStateUnavailableError(
      `the first block's prev_utxos_root ${expectedRoot} is not the empty UTxO set root`,
    );
  }
  const retained = await getDaPayload(header.prevHeaderHash);
  if (retained !== undefined && retained.payloadCborHex.length > 0) {
    const utxos = await boundUtxos(
      Buffer.from(retained.payloadCborHex, "hex"),
      expectedRoot,
    );
    if (utxos !== undefined) return toPreBlockUtxos(utxos);
  }
  const fetched = await payloadSource.fetchPayloadCandidates(
    header.prevHeaderHash,
  );
  if (fetched.ok) {
    for (const candidate of fetched.candidates) {
      const utxos = await boundUtxos(candidate.payloadCbor, expectedRoot);
      if (utxos !== undefined) return toPreBlockUtxos(utxos);
    }
  }
  throw new ParentStateUnavailableError(
    `parent DA payload ${header.prevHeaderHash} with utxos_root ${expectedRoot} is not available; the block is not attested until it is`,
  );
};
