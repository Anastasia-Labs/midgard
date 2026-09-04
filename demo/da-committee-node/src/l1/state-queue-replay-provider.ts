import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

type Queue = readonly SDK.StateQueueTransitionNode[];
type Point = Readonly<{ slot: number; blockHash: string }>;
type Spend = Readonly<{ transactionHash: string; point: Point }>;
type Transaction = Readonly<{
  transactionHash: string;
  blockHash: string;
  slot: number;
  blockNo: number;
  transactionIndex: number;
  mintPolicyIds: readonly string[];
  redeemers: readonly SDK.StateQueueTransitionRedeemer[];
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
}>;
type HistoricalOutput = Readonly<{
  node: SDK.StateQueueTransitionNode;
  nextHeaderHash: string | null;
}>;

export type StateQueueReplayFetch = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type StateQueueReplayWebSocket = Readonly<{
  send(data: string): void;
  close(code?: number, reason?: string): void;
  addEventListener(
    type: string,
    listener: (event: never) => void,
    options?: { once?: boolean },
  ): void;
}>;

export type StateQueueReplayWebSocketFactory = (
  url: string,
) => StateQueueReplayWebSocket;

const stableJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${stableJson(member)}`)
    .join(",")}}`;
};
const digest = (value: unknown): string =>
  createHash("sha256").update(stableJson(value)).digest("hex");
const sameQueue = (left: Queue, right: Queue): boolean =>
  left.length === right.length &&
  left.every(
    (node, index) =>
      node.headerHash === right[index]?.headerHash &&
      node.outRef === right[index]?.outRef,
  );
const splitOutRef = (value: string): { txHash: string; index: number } => {
  if (!OUT_REF.test(value))
    throw new Error("state-queue replay outref is invalid");
  const [txHash, index] = value.split("#") as [string, string];
  return { txHash, index: Number(index) };
};
const httpUrl = (value: string): string => {
  const url = new URL(value);
  if (url.protocol === "ws:") url.protocol = "http:";
  if (url.protocol === "wss:") url.protocol = "https:";
  url.hash = "";
  return url.toString().replace(/\/$/u, "");
};
const wsUrl = (value: string): string => {
  const url = new URL(value);
  if (url.protocol === "http:") url.protocol = "ws:";
  if (url.protocol === "https:") url.protocol = "wss:";
  url.hash = "";
  return url.toString().replace(/\/$/u, "");
};
const json = async (
  fetchImpl: StateQueueReplayFetch,
  url: string,
  init?: RequestInit,
): Promise<unknown> => {
  const response = await fetchImpl(url, init);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `state-queue replay HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  try {
    return JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("state-queue replay source returned malformed JSON", {
      cause,
    });
  }
};
const point = (value: unknown, label: string): Point => {
  const candidate = value as { slot_no?: unknown; header_hash?: unknown };
  if (
    typeof candidate.slot_no !== "number" ||
    !Number.isSafeInteger(candidate.slot_no) ||
    candidate.slot_no < 0 ||
    typeof candidate.header_hash !== "string" ||
    !HEX_32.test(candidate.header_hash)
  ) {
    throw new Error(`${label} is not a canonical chain point`);
  }
  return { slot: candidate.slot_no, blockHash: candidate.header_hash };
};

const fetchSpend = async (
  kupoUrl: string,
  reference: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<Spend | null> => {
  const { txHash, index } = splitOutRef(reference);
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/${index.toString()}@${txHash}?resolve_hashes`,
  );
  if (!Array.isArray(body))
    throw new Error("Kupo spend lookup is not an array");
  const matches = body.filter(
    (item) =>
      (item as { transaction_id?: unknown }).transaction_id === txHash &&
      (item as { output_index?: unknown }).output_index === index,
  );
  if (matches.length !== 1) throw new Error("Kupo spend lookup is not unique");
  const match = matches[0] as { datum?: unknown; spent_at?: unknown };
  if (!("datum" in match)) {
    throw new Error("Kupo replay requires resolve_hashes support");
  }
  if (match.spent_at === null) return null;
  if (typeof match.spent_at !== "object" || match.spent_at === undefined) {
    throw new Error("Kupo spend lookup omitted spent_at");
  }
  const spent = match.spent_at as { transaction_id?: unknown };
  if (
    typeof spent.transaction_id !== "string" ||
    !HEX_32.test(spent.transaction_id)
  ) {
    throw new Error("Kupo spent_at transaction id is invalid");
  }
  return {
    transactionHash: spent.transaction_id,
    point: point(spent, "spent_at"),
  };
};

const fetchAncestor = async (
  kupoUrl: string,
  slot: number,
  fetchImpl: StateQueueReplayFetch,
): Promise<Point> => {
  if (slot < 1) throw new Error("genesis has no replay ancestor");
  return point(
    await json(
      fetchImpl,
      `${httpUrl(kupoUrl)}/checkpoints/${(slot - 1).toString()}`,
    ),
    "Kupo replay ancestor",
  );
};

type Rpc = Readonly<{
  request(method: string, params: Record<string, unknown>): Promise<unknown>;
  close(): void;
}>;
const openRpc = async (
  url: string,
  factory: StateQueueReplayWebSocketFactory,
): Promise<Rpc> => {
  const socket = factory(wsUrl(url));
  const pending = new Map<
    number,
    { resolve(value: unknown): void; reject(error: Error): void }
  >();
  let nextId = 0;
  let terminal: Error | null = null;
  const fail = (error: Error): void => {
    terminal ??= error;
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
  };
  socket.addEventListener("message", ((event: { data: unknown }) => {
    if (typeof event.data !== "string")
      return fail(new Error("Ogmios returned binary data"));
    let response: { id?: unknown; result?: unknown; error?: unknown };
    try {
      response = JSON.parse(event.data) as typeof response;
    } catch (cause) {
      return fail(new Error("Ogmios returned malformed JSON", { cause }));
    }
    if (typeof response.id !== "number") return;
    const waiter = pending.get(response.id);
    if (waiter === undefined) return;
    pending.delete(response.id);
    if (response.error !== undefined) {
      waiter.reject(
        new Error(`Ogmios replay error: ${JSON.stringify(response.error)}`),
      );
    } else {
      waiter.resolve(response.result);
    }
  }) as (event: never) => void);
  socket.addEventListener("error", (() =>
    fail(new Error("Ogmios replay socket failed"))) as (event: never) => void);
  socket.addEventListener("close", (() =>
    fail(new Error("Ogmios replay socket closed"))) as (event: never) => void);
  await new Promise<void>((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error("Ogmios replay socket open timed out")),
      20_000,
    );
    socket.addEventListener(
      "open",
      (() => {
        clearTimeout(timer);
        resolve();
      }) as (event: never) => void,
      { once: true },
    );
    socket.addEventListener(
      "error",
      (() => {
        clearTimeout(timer);
        reject(new Error("Ogmios replay socket failed while opening"));
      }) as (event: never) => void,
      { once: true },
    );
  });
  return {
    request: async (method, params) => {
      if (terminal !== null) throw terminal;
      const id = nextId++;
      return await new Promise<unknown>((resolve, reject) => {
        const timer = setTimeout(() => {
          pending.delete(id);
          reject(new Error(`Ogmios ${method} replay timed out`));
        }, 20_000);
        pending.set(id, {
          resolve: (value) => {
            clearTimeout(timer);
            resolve(value);
          },
          reject: (error) => {
            clearTimeout(timer);
            reject(error);
          },
        });
        socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
      });
    },
    close: () => socket.close(),
  };
};

const parseTransaction = (
  value: unknown,
  block: { blockHash: string; slot: number; blockNo: number },
  transactionIndex: number,
): Transaction => {
  const tx = value as {
    id?: unknown;
    inputs?: unknown;
    references?: unknown;
    mint?: unknown;
    redeemers?: unknown;
  };
  if (typeof tx.id !== "string" || !HEX_32.test(tx.id)) {
    throw new Error("Ogmios replay transaction id is invalid");
  }
  if (!Array.isArray(tx.inputs))
    throw new Error("Ogmios replay inputs are absent");
  const spentInputOutRefs = tx.inputs.map((item) => {
    const input = item as { transaction?: { id?: unknown }; index?: unknown };
    if (
      typeof input.transaction?.id !== "string" ||
      !HEX_32.test(input.transaction.id) ||
      typeof input.index !== "number" ||
      !Number.isSafeInteger(input.index) ||
      input.index < 0
    ) {
      throw new Error("Ogmios replay input is invalid");
    }
    return `${input.transaction.id}#${input.index.toString()}`;
  });
  const references = tx.references ?? [];
  if (!Array.isArray(references)) {
    throw new Error("Ogmios replay reference inputs are invalid");
  }
  const referenceInputOutRefs = references.map((item) => {
    const input = item as { transaction?: { id?: unknown }; index?: unknown };
    if (
      typeof input.transaction?.id !== "string" ||
      !HEX_32.test(input.transaction.id) ||
      typeof input.index !== "number" ||
      !Number.isSafeInteger(input.index) ||
      input.index < 0
    ) {
      throw new Error("Ogmios replay reference input is invalid");
    }
    return `${input.transaction.id}#${input.index.toString()}`;
  });
  const mint = tx.mint ?? {};
  if (typeof mint !== "object" || mint === null || Array.isArray(mint)) {
    throw new Error("Ogmios replay mint is invalid");
  }
  const mintPolicyIds = Object.keys(mint);
  if (mintPolicyIds.some((policyId) => !HEX_28.test(policyId))) {
    throw new Error("Ogmios replay mint policy id is invalid");
  }
  mintPolicyIds.sort();
  const redeemers = tx.redeemers ?? [];
  if (!Array.isArray(redeemers))
    throw new Error("Ogmios replay redeemers are invalid");
  const parsedRedeemers = redeemers.map((item) => {
    const redeemer = item as {
      redeemer?: unknown;
      validator?: { purpose?: unknown; index?: unknown };
    };
    if (
      typeof redeemer.redeemer !== "string" ||
      typeof redeemer.validator?.purpose !== "string" ||
      typeof redeemer.validator.index !== "number" ||
      !Number.isSafeInteger(redeemer.validator.index) ||
      redeemer.validator.index < 0
    ) {
      throw new Error("Ogmios replay redeemer is invalid");
    }
    return {
      purpose: redeemer.validator.purpose,
      index: redeemer.validator.index.toString(),
      cborHex: redeemer.redeemer,
    };
  });
  return {
    transactionHash: tx.id,
    ...block,
    transactionIndex,
    mintPolicyIds,
    redeemers: parsedRedeemers,
    spentInputOutRefs,
    referenceInputOutRefs,
  };
};

const readTransaction = async (
  ogmiosUrl: string,
  ancestor: Point,
  spend: Spend,
  factory: StateQueueReplayWebSocketFactory,
): Promise<Transaction> => {
  const rpc = await openRpc(ogmiosUrl, factory);
  try {
    const found = (await rpc.request("findIntersection", {
      points: [{ slot: ancestor.slot, id: ancestor.blockHash }],
    })) as { intersection?: unknown };
    if (found.intersection === undefined)
      throw new Error("Ogmios found no replay intersection");
    let handshakeRollback = false;
    for (let scanned = 0; scanned < 1_000; scanned += 1) {
      const next = (await rpc.request("nextBlock", {})) as {
        direction?: unknown;
        block?: unknown;
      };
      if (next.direction === "backward" && !handshakeRollback) {
        handshakeRollback = true;
        scanned -= 1;
        continue;
      }
      if (next.direction === "backward")
        throw new Error("Ogmios rolled back during replay");
      if (next.direction !== "forward")
        throw new Error("Ogmios replay direction is invalid");
      const block = next.block as {
        id?: unknown;
        slot?: unknown;
        height?: unknown;
        transactions?: unknown;
      };
      if (block.id !== spend.point.blockHash) {
        if (typeof block.slot === "number" && block.slot > spend.point.slot) {
          throw new Error("Ogmios replay passed the Kupo spend point");
        }
        continue;
      }
      if (
        typeof block.id !== "string" ||
        !HEX_32.test(block.id) ||
        typeof block.slot !== "number" ||
        !Number.isSafeInteger(block.slot) ||
        block.slot < 0 ||
        typeof block.height !== "number" ||
        !Number.isSafeInteger(block.height) ||
        block.height < 0 ||
        !Array.isArray(block.transactions)
      ) {
        throw new Error("Ogmios replay block is invalid");
      }
      const index = block.transactions.findIndex(
        (item) => (item as { id?: unknown }).id === spend.transactionHash,
      );
      if (index < 0)
        throw new Error("Kupo spend transaction is absent from Ogmios block");
      return parseTransaction(
        block.transactions[index],
        { blockHash: block.id, slot: block.slot, blockNo: block.height },
        index,
      );
    }
    throw new Error("Ogmios replay exceeded its block scan bound");
  } finally {
    rpc.close();
  }
};

const fetchOutputs = async (
  kupoUrl: string,
  transactionHash: string,
  stateQueueAddress: string,
  stateQueuePolicyId: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<readonly HistoricalOutput[]> => {
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/*@${transactionHash}?resolve_hashes`,
  );
  if (!Array.isArray(body))
    throw new Error("Kupo transaction output lookup is invalid");
  const outputs: HistoricalOutput[] = [];
  for (const item of body) {
    const output = item as {
      transaction_id?: unknown;
      output_index?: unknown;
      address?: unknown;
      datum_type?: unknown;
      datum?: unknown;
      value?: { assets?: unknown };
    };
    if (
      output.transaction_id !== transactionHash ||
      typeof output.output_index !== "number" ||
      !Number.isSafeInteger(output.output_index) ||
      output.output_index < 0 ||
      typeof output.value?.assets !== "object" ||
      output.value.assets === null ||
      Array.isArray(output.value.assets)
    ) {
      throw new Error("Kupo replay transaction output is invalid");
    }
    const assets = Object.entries(
      output.value.assets as Record<string, unknown>,
    ).flatMap(([unit, quantity]) => {
      const normalized = unit.replaceAll(".", "");
      return normalized.startsWith(stateQueuePolicyId)
        ? [{ assetName: normalized.slice(stateQueuePolicyId.length), quantity }]
        : [];
    });
    if (assets.length === 0) continue;
    if (
      output.address !== stateQueueAddress ||
      assets.length !== 1 ||
      (assets[0]!.quantity !== 1 && assets[0]!.quantity !== "1") ||
      output.datum_type !== "inline" ||
      typeof output.datum !== "string"
    ) {
      throw new Error("state-queue policy output has invalid provenance");
    }
    const assetName = assets[0]!.assetName;
    const headerHash =
      assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME
        ? null
        : assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
            HEX_28.test(
              assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
            )
          ? assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length)
          : undefined;
    if (headerHash === undefined)
      throw new Error("unknown state-queue asset name");
    let view: SDK.LinkedListNodeView;
    try {
      view = SDK.linkedListDatumToNodeView(
        Data.from(output.datum, SDK.LinkedListDatum),
        assetName,
      );
    } catch (cause) {
      throw new Error("state-queue replay datum is invalid", { cause });
    }
    const viewHeader = view.key === "Empty" ? null : view.key.Key.key;
    if (viewHeader !== headerHash)
      throw new Error("state-queue asset and datum disagree");
    outputs.push({
      node: {
        headerHash,
        outRef: `${transactionHash}#${output.output_index.toString()}`,
      },
      nextHeaderHash: view.next === "Empty" ? null : view.next.Key.key,
    });
  }
  return outputs;
};

type CorrectionLockOutput = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
}>;

const decodeCorrectionLockOutput = (
  value: unknown,
  transactionHash: string,
  outputIndex: number,
  correctionLockAddress: string,
  hubOraclePolicyId: string,
): CorrectionLockOutput | null => {
  const output = value as {
    transaction_id?: unknown;
    output_index?: unknown;
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    output.transaction_id !== transactionHash ||
    output.output_index !== outputIndex ||
    output.address !== correctionLockAddress ||
    output.datum_type !== "inline" ||
    typeof output.datum !== "string" ||
    typeof output.value?.assets !== "object" ||
    output.value.assets === null ||
    Array.isArray(output.value.assets)
  ) {
    return null;
  }
  const assets = Object.entries(
    output.value.assets as Record<string, unknown>,
  ).map(([unit, quantity]) => [unit.replaceAll(".", ""), quantity] as const);
  if (
    assets.length !== 1 ||
    assets[0]?.[0] !== SDK.correctionLockUnit(hubOraclePolicyId) ||
    (assets[0]?.[1] !== 1 && assets[0]?.[1] !== "1")
  ) {
    return null;
  }
  try {
    return {
      outRef: `${transactionHash}#${outputIndex.toString()}`,
      datum: Data.from(output.datum, SDK.CorrectionLockDatum),
    };
  } catch {
    return null;
  }
};

const fetchResolvedOutput = async (
  kupoUrl: string,
  reference: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<unknown> => {
  const { txHash, index } = splitOutRef(reference);
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/${index.toString()}@${txHash}?resolve_hashes`,
  );
  if (!Array.isArray(body)) {
    throw new Error("Kupo resolved replay output is not an array");
  }
  const matches = body.filter(
    (item) =>
      (item as { transaction_id?: unknown }).transaction_id === txHash &&
      (item as { output_index?: unknown }).output_index === index,
  );
  if (matches.length !== 1) {
    throw new Error("Kupo resolved replay output is not unique");
  }
  return matches[0];
};

const fetchCorrectionLockOutputs = async (
  kupoUrl: string,
  transactionHash: string,
  correctionLockAddress: string,
  hubOraclePolicyId: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<readonly CorrectionLockOutput[]> => {
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/*@${transactionHash}?resolve_hashes`,
  );
  if (!Array.isArray(body)) {
    throw new Error("Kupo correction-lock replay outputs are not an array");
  }
  return body.flatMap((item) => {
    const index = (item as { output_index?: unknown }).output_index;
    if (
      typeof index !== "number" ||
      !Number.isSafeInteger(index) ||
      index < 0
    ) {
      return [];
    }
    const lock = decodeCorrectionLockOutput(
      item,
      transactionHash,
      index,
      correctionLockAddress,
      hubOraclePolicyId,
    );
    return lock === null ? [] : [lock];
  });
};

const fraudProofAssetName = (
  value: unknown,
  fraudProofAddress: string,
  fraudProofPolicyId: string,
  targetHeaderHash: string,
): string | null => {
  const output = value as {
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    output.address !== fraudProofAddress ||
    output.datum_type !== "inline" ||
    typeof output.datum !== "string" ||
    typeof output.value?.assets !== "object" ||
    output.value.assets === null ||
    Array.isArray(output.value.assets)
  ) {
    return null;
  }
  const names = Object.entries(
    output.value.assets as Record<string, unknown>,
  ).flatMap(([unit, quantity]) => {
    const normalized = unit.replaceAll(".", "");
    if (!normalized.startsWith(fraudProofPolicyId)) return [];
    const assetName = normalized.slice(fraudProofPolicyId.length);
    return /^[0-9a-f]{64}$/u.test(assetName) &&
      assetName.slice(8) === targetHeaderHash &&
      (quantity === 1 || quantity === "1")
      ? [assetName]
      : [null];
  });
  return names.length === 1 ? names[0] : null;
};

const correctionLockWitness = async ({
  transaction,
  outputs,
  stateQueuePolicyId,
  hubOraclePolicyId,
  correctionLockAddress,
  fraudProofPolicyId,
  fraudProofAddress,
  kupoUrl,
  fetchImpl,
}: {
  readonly transaction: Transaction;
  readonly outputs: readonly CorrectionLockOutput[];
  readonly stateQueuePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly kupoUrl: string;
  readonly fetchImpl: StateQueueReplayFetch;
}): Promise<SDK.StateQueueCorrectionLockWitness> => {
  const spentResolved = await Promise.all(
    transaction.spentInputOutRefs.map(async (reference) => ({
      reference,
      value: await fetchResolvedOutput(kupoUrl, reference, fetchImpl),
    })),
  );
  const referenceResolved = await Promise.all(
    transaction.referenceInputOutRefs.map(async (reference) => ({
      reference,
      value: await fetchResolvedOutput(kupoUrl, reference, fetchImpl),
    })),
  );
  const locksIn = spentResolved.flatMap(({ reference, value }) => {
    const { txHash, index } = splitOutRef(reference);
    const lock = decodeCorrectionLockOutput(
      value,
      txHash,
      index,
      correctionLockAddress,
      hubOraclePolicyId,
    );
    return lock === null ? [] : [lock];
  });
  const locksReferenced = referenceResolved.flatMap(({ reference, value }) => {
    const { txHash, index } = splitOutRef(reference);
    const lock = decodeCorrectionLockOutput(
      value,
      txHash,
      index,
      correctionLockAddress,
      hubOraclePolicyId,
    );
    return lock === null ? [] : [lock];
  });
  const policyIndex = transaction.mintPolicyIds.indexOf(stateQueuePolicyId);
  if (policyIndex < 0) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      outputs.length !== 0
    ) {
      throw new Error(
        "non-mint checkpoint unexpectedly carries CorrectionLock",
      );
    }
    return { kind: "none" };
  }
  const mint = transaction.redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (mint.length !== 1)
    throw new Error("state-queue mint redeemer is not unique");
  let decoded: SDK.StateQueueRedeemer;
  try {
    decoded = Data.from(mint[0]!.cborHex, SDK.StateQueueRedeemer);
  } catch (cause) {
    throw new Error("state-queue mint redeemer is invalid", { cause });
  }
  if (typeof decoded === "object" && decoded !== null && "InitV1" in decoded) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      outputs.length !== 1 ||
      outputs[0]!.datum !== "Idle"
    ) {
      throw new Error("state-queue init CorrectionLock topology is invalid");
    }
    return {
      kind: "genesis",
      producedOutRef: outputs[0]!.outRef,
      nextDatum: "Idle",
    };
  }
  if (decoded === "Deinit") {
    if (
      locksIn.length !== 1 ||
      locksIn[0]!.datum !== "Idle" ||
      locksReferenced.length !== 0 ||
      outputs.length !== 0
    ) {
      throw new Error("state-queue deinit CorrectionLock topology is invalid");
    }
    return {
      kind: "deinit",
      consumedOutRef: locksIn[0]!.outRef,
      previousDatum: "Idle",
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("CommitBlockHeader" in decoded || "MergeToConfirmedStateV1" in decoded)
  ) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 1 ||
      locksReferenced[0]!.datum !== "Idle" ||
      outputs.length !== 0
    ) {
      throw new Error("append/merge CorrectionLock topology is invalid");
    }
    return {
      kind: "idle_reference",
      referenceOutRef: locksReferenced[0]!.outRef,
      datum: "Idle",
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveFraudulentBlockHeader" in decoded)
  ) {
    if (
      locksIn.length !== 1 ||
      locksReferenced.length !== 0 ||
      outputs.length !== 1
    ) {
      throw new Error("correction CorrectionLock topology is invalid");
    }
    const targetHeaderHash =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? decoded.RemoveUnattestedBlockAfterTimeout.timed_out_header_hash
        : decoded.RemoveFraudulentBlockHeader.fraudulent_blocks_header_hash;
    const identity: SDK.CorrectionIdentity =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? "AttestationTimeout"
        : (() => {
            const reference =
              referenceResolved[
                Number(
                  decoded.RemoveFraudulentBlockHeader
                    .fraud_proof_ref_input_index,
                )
              ];
            const assetName =
              reference === undefined
                ? null
                : fraudProofAssetName(
                    reference.value,
                    fraudProofAddress,
                    fraudProofPolicyId,
                    targetHeaderHash,
                  );
            if (assetName === null)
              throw new Error("fraud proof CorrectionLock identity is invalid");
            return { FraudProof: { fraud_proof_asset_name: assetName } };
          })();
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: outputs[0]!.outRef,
      targetHeaderHash,
      correctionIdentity: identity,
      previousDatum: locksIn[0]!.datum,
      nextDatum: outputs[0]!.datum,
    };
  }
  throw new Error("state-queue checkpoint has no CorrectionLock topology");
};

const reconstruct = (
  previousQueue: Queue,
  transaction: Transaction,
  outputs: readonly HistoricalOutput[],
): Queue => {
  const spent = new Set(transaction.spentInputOutRefs);
  const previousByIdentity = new Map(
    previousQueue.map((node) => [node.headerHash, node]),
  );
  const outputByIdentity = new Map(
    outputs.map(({ node }) => [node.headerHash, node]),
  );
  if (
    outputs.length === 0 ||
    !previousQueue.some(({ outRef }) => spent.has(outRef)) ||
    outputs.some(
      ({ node }) =>
        previousByIdentity.has(node.headerHash) &&
        !spent.has(previousByIdentity.get(node.headerHash)!.outRef),
    )
  ) {
    throw new Error("state-queue replay outputs do not follow their inputs");
  }
  const retained = previousQueue.flatMap((node) => {
    if (!spent.has(node.outRef)) return [node];
    const continuation = outputByIdentity.get(node.headerHash);
    return continuation === undefined ? [] : [continuation];
  });
  const introduced = outputs
    .map(({ node }) => node)
    .filter(({ headerHash }) => !previousByIdentity.has(headerHash));
  if (
    introduced.length > 1 ||
    introduced.some(({ headerHash }) => headerHash === null)
  ) {
    throw new Error("state-queue replay introduced invalid identities");
  }
  const nextQueue = [...retained, ...introduced];
  if (
    nextQueue.length === 0 ||
    nextQueue[0]?.headerHash !== null ||
    new Set(nextQueue.map(({ headerHash }) => headerHash)).size !==
      nextQueue.length ||
    new Set(nextQueue.map(({ outRef }) => outRef)).size !== nextQueue.length
  ) {
    throw new Error("state-queue replay reconstructed an invalid queue");
  }
  const expectedLinks = new Map(
    nextQueue.map((node, index) => [
      node.headerHash,
      nextQueue[index + 1]?.headerHash ?? null,
    ]),
  );
  if (
    outputs.some(
      ({ node, nextHeaderHash }) =>
        expectedLinks.get(node.headerHash) !== nextHeaderHash,
    )
  ) {
    throw new Error("state-queue replay linked-list outputs disagree");
  }
  return nextQueue;
};

const fetchTipBlockNo = async (
  ogmiosUrl: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<number> => {
  const body = (await json(fetchImpl, httpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryNetwork/tip",
      params: {},
      id: "midgard-committee-state-queue-replay-tip-v1",
    }),
  })) as { result?: { height?: unknown; tip?: { height?: unknown } } };
  const height = body.result?.tip?.height ?? body.result?.height;
  if (
    typeof height !== "number" ||
    !Number.isSafeInteger(height) ||
    height < 0
  ) {
    throw new Error("Ogmios replay tip height is invalid");
  }
  return height;
};

/** Independent committee-side local Kupmios ordered state-queue replay. */
export const createLocalKupmiosStateQueueReplayProvider = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  stateQueueAddress,
  hubOraclePolicyId,
  correctionLockAddress,
  fraudProofPolicyId,
  fraudProofAddress,
  kupoUrl,
  ogmiosUrl,
  fetchImpl = fetch,
  webSocketFactory = (url) =>
    new WebSocket(url) as unknown as StateQueueReplayWebSocket,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly fetchImpl?: StateQueueReplayFetch;
  readonly webSocketFactory?: StateQueueReplayWebSocketFactory;
}): ((
  previousQueue: Queue,
  currentQueue: Queue,
) => Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]>) => {
  if (
    !HEX_32.test(deploymentIdentityDigest) ||
    !HEX_28.test(stateQueuePolicyId) ||
    !HEX_28.test(hubOraclePolicyId) ||
    !HEX_28.test(fraudProofPolicyId) ||
    correctionLockAddress.trim() === "" ||
    fraudProofAddress.trim() === ""
  ) {
    throw new Error("committee state-queue replay release identity is invalid");
  }
  return async (previousQueue, currentQueue) => {
    let queue = previousQueue;
    const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
    const tipBlockNo = await fetchTipBlockNo(ogmiosUrl, fetchImpl);
    for (let replayed = 0; replayed < 1_000; replayed += 1) {
      if (sameQueue(queue, currentQueue)) return checkpoints;
      const spends = await Promise.all(
        queue.map(({ outRef }) => fetchSpend(kupoUrl, outRef, fetchImpl)),
      );
      const unique = new Map<string, Spend>();
      for (const spend of spends) {
        if (spend === null) continue;
        const prior = unique.get(spend.transactionHash);
        if (
          prior !== undefined &&
          (prior.point.slot !== spend.point.slot ||
            prior.point.blockHash !== spend.point.blockHash)
        ) {
          throw new Error(
            "Kupo replay assigned one transaction to competing points",
          );
        }
        unique.set(spend.transactionHash, spend);
      }
      if (unique.size === 0)
        throw new Error("committee replay cannot advance its durable queue");
      const transactions = await Promise.all(
        [...unique.values()].map(
          async (spend) =>
            await readTransaction(
              ogmiosUrl,
              await fetchAncestor(kupoUrl, spend.point.slot, fetchImpl),
              spend,
              webSocketFactory,
            ),
        ),
      );
      transactions.sort(
        (left, right) =>
          left.blockNo - right.blockNo ||
          left.transactionIndex - right.transactionIndex ||
          left.transactionHash.localeCompare(right.transactionHash),
      );
      const transaction = transactions[0]!;
      if (tipBlockNo < transaction.blockNo) {
        throw new Error(
          "Ogmios replay tip precedes the observed queue transaction",
        );
      }
      const outputs = await fetchOutputs(
        kupoUrl,
        transaction.transactionHash,
        stateQueueAddress,
        stateQueuePolicyId,
        fetchImpl,
      );
      const nextQueue = reconstruct(queue, transaction, outputs);
      const lockOutputs = await fetchCorrectionLockOutputs(
        kupoUrl,
        transaction.transactionHash,
        correctionLockAddress,
        hubOraclePolicyId,
        fetchImpl,
      );
      const lockWitness = await correctionLockWitness({
        transaction,
        outputs: lockOutputs,
        stateQueuePolicyId,
        hubOraclePolicyId,
        correctionLockAddress,
        fraudProofPolicyId,
        fraudProofAddress,
        kupoUrl,
        fetchImpl,
      });
      const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
        deploymentIdentityDigest,
        stateQueuePolicyId,
        transactionHash: transaction.transactionHash,
        blockHash: transaction.blockHash,
        slot: transaction.slot.toString(),
        blockNo: transaction.blockNo.toString(),
        transactionIndex: transaction.transactionIndex.toString(),
        chainPointId: digest({
          source: "da-committee-local-kupmios-state-queue-replay-v1",
          blockHash: transaction.blockHash,
          slot: transaction.slot,
          blockNo: transaction.blockNo,
          transactionIndex: transaction.transactionIndex,
        }),
        finalityDepth: (tipBlockNo - transaction.blockNo + 1).toString(),
        mintPolicyIds: transaction.mintPolicyIds,
        redeemers: transaction.redeemers,
        spentInputOutRefs: transaction.spentInputOutRefs,
        referenceInputOutRefs: transaction.referenceInputOutRefs,
        correctionLockWitness: lockWitness,
        previousQueue: queue,
        nextQueue,
      });
      if (checkpoint === null) {
        throw new Error(
          "committee state-queue transaction failed exact checkpoint derivation",
        );
      }
      checkpoints.push(checkpoint);
      queue = nextQueue;
    }
    throw new Error("committee state-queue replay exceeded its safety bound");
  };
};
