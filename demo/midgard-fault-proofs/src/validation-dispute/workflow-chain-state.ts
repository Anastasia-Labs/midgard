import type { MidgardValidationDispute } from "@al-ft/midgard-core";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { ValidationTraceDisputeFaultProofContracts } from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadStepDatum,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  PendingValidationClaimDatum,
  PreparedValidationResolutionDatum,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  utxoToStateQueueUTxO,
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
  ValidationResolutionDatum,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Ruling R2: the family-local dispute cursor. Every stage is derived
 * exclusively from on-chain state — the live computation-thread token's
 * address and inline datum — never from local memory. The interactive
 * bisection stage exposes an explicit turn indicator and depth cursor, and
 * multi-transaction semantic routes surface their sub-chain checkpoint
 * position so an interrupted runner resumes from re-derived chain state.
 */
export type ValidationTraceDisputeSemanticGroup =
  | "cek_material_traversal"
  | "cek_core_stage"
  | "cek_context_stage"
  | "cek_context_item_stage"
  | "canonical_decode_item_stage"
  | "script_sources_item_stage"
  | "proof_item";

export type ValidationTraceDisputeChainStage =
  | Readonly<{ kind: "not_started"; stateQueueBlockOutRef: string }>
  | Readonly<{
      kind: "init";
      threadOutRef: string;
      /** Authenticated topology target; patched in by the derivation. */
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ kind: "open_pending_source"; threadOutRef: string }>
  | Readonly<{
      kind: "game";
      threadOutRef: string;
      dispute: MidgardValidationDispute;
      turn: "awaiting_operator" | "awaiting_challenger" | "ready_for_one_step";
      round: number;
      lowIndex: number;
      highIndex: number;
      responseDeadline: number;
      /** The counterparty stalled past its deadline; enter-timeout is legal. */
      timeoutClaimable: boolean;
    }>
  | Readonly<{ kind: "timeout_pending"; threadOutRef: string }>
  | Readonly<{ kind: "resolution_boundary"; threadOutRef: string }>
  | Readonly<{
      kind: "prepare_selected_pending";
      threadOutRef: string;
      resolverIndex: number;
    }>
  | Readonly<{
      kind: "semantic_pending";
      threadOutRef: string;
      semanticResolverGlobalIndex: number;
    }>
  | Readonly<{
      kind: "semantic_in_flight";
      threadOutRef: string;
      group: ValidationTraceDisputeSemanticGroup;
      role: string;
    }>
  | Readonly<{ kind: "award_pending"; threadOutRef: string }>
  | Readonly<{
      kind: "proof_token";
      fraudProofOutRef: string;
      nextRemovalOutRef: string;
    }>
  | Readonly<{ kind: "removed" }>;

type FamilyChain =
  ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];

type AddressEntry = Readonly<{
  classify: (
    utxo: UTxO,
    currentTime: number,
  ) => ValidationTraceDisputeChainStage;
}>;

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const requireDatum = (utxo: UTxO, label: string): string => {
  if (utxo.datum == null) {
    throw new Error(
      `validationTraceDispute ${label} thread ${outRef(utxo)} is missing its inline datum`,
    );
  }
  return utxo.datum;
};

const gameStage = (
  utxo: UTxO,
  currentTime: number,
): ValidationTraceDisputeChainStage => {
  const datum = Data.from(requireDatum(utxo, "game"), ValidationDisputeDatum);
  if (datum.data === null) {
    throw new Error(
      `validationTraceDispute game thread ${outRef(utxo)} carries a null dispute state`,
    );
  }
  const dispute = validationDisputeCoreFromData(datum.data.dispute);
  const turn =
    dispute.turn.type === "awaitingOperator"
      ? ("awaiting_operator" as const)
      : dispute.turn.type === "awaitingChallenger"
        ? ("awaiting_challenger" as const)
        : ("ready_for_one_step" as const);
  return {
    kind: "game",
    threadOutRef: outRef(utxo),
    dispute,
    turn,
    round: dispute.round,
    lowIndex: dispute.lowIndex,
    highIndex: dispute.highIndex,
    responseDeadline: dispute.responseDeadline,
    timeoutClaimable:
      turn === "awaiting_operator" && currentTime > dispute.responseDeadline,
  };
};

const decodeOnly =
  (
    kind:
      | "open_pending_source"
      | "timeout_pending"
      | "resolution_boundary"
      | "award_pending",
    decode: (datumCbor: string) => unknown,
  ) =>
  (utxo: UTxO): ValidationTraceDisputeChainStage => {
    decode(requireDatum(utxo, kind));
    return { kind, threadOutRef: outRef(utxo) };
  };

const inFlight =
  (group: ValidationTraceDisputeSemanticGroup, role: string) =>
  (utxo: UTxO): ValidationTraceDisputeChainStage => ({
    kind: "semantic_in_flight",
    threadOutRef: outRef(utxo),
    group,
    role,
  });

const groupAddresses = (
  value: unknown,
  visit: (address: string, role: string) => void,
  role: string,
): void => {
  if (value === null || typeof value !== "object") return;
  const candidate = value as Readonly<Record<string, unknown>>;
  if (typeof candidate.spendingScriptAddress === "string") {
    visit(candidate.spendingScriptAddress, role);
    return;
  }
  for (const [key, child] of Object.entries(candidate)) {
    groupAddresses(child, visit, role === "" ? key : `${role}.${key}`);
  }
};

/**
 * Deterministic address classifier over the resolved family chain. Control
 * stages are registered first and win over any later group that compiles to
 * the same address (none do today; the guard keeps the first, most specific
 * classification if a future chain revision aliases addresses).
 */
export const buildValidationTraceDisputeAddressClassifier = (
  chain: FamilyChain,
): ReadonlyMap<string, AddressEntry> => {
  const map = new Map<string, AddressEntry>();
  const put = (address: string, classify: AddressEntry["classify"]): void => {
    if (!map.has(address)) map.set(address, { classify });
  };
  put(chain.opener.spendingScriptAddress, (utxo) => {
    Data.from(requireDatum(utxo, "init"), FraudProofComputationThreadStepDatum);
    // The derivation replaces the placeholder with the authenticated
    // state-queue topology target before this stage is ever surfaced.
    return {
      kind: "init",
      threadOutRef: outRef(utxo),
      stateQueueBlockOutRef: "",
    };
  });
  put(chain.source.spendingScriptAddress, (utxo) =>
    decodeOnly("open_pending_source", (cbor) =>
      Data.from(cbor, PendingValidationClaimDatum),
    )(utxo),
  );
  put(chain.game.spendingScriptAddress, gameStage);
  put(chain.boundary.spendingScriptAddress, (utxo) =>
    decodeOnly("resolution_boundary", (cbor) =>
      Data.from(cbor, ValidationDisputeDatum),
    )(utxo),
  );
  put(chain.timeout.spendingScriptAddress, (utxo) =>
    decodeOnly("timeout_pending", (cbor) =>
      Data.from(cbor, ValidationDisputeDatum),
    )(utxo),
  );
  put(chain.award.spendingScriptAddress, (utxo) =>
    decodeOnly("award_pending", (cbor) =>
      Data.from(cbor, WinningValidationResolutionDatum),
    )(utxo),
  );
  chain.prepareResolvers.forEach((resolver, resolverIndex) => {
    put(resolver.spendingScriptAddress, (utxo) => {
      Data.from(
        requireDatum(utxo, "prepare resolver"),
        ValidationResolutionDatum,
      );
      return {
        kind: "prepare_selected_pending",
        threadOutRef: outRef(utxo),
        resolverIndex,
      };
    });
  });
  chain.semanticResolvers.forEach((resolver, globalIndex) => {
    put(resolver.spendingScriptAddress, (utxo) => {
      Data.from(
        requireDatum(utxo, "semantic resolver"),
        PreparedValidationResolutionDatum,
      );
      return {
        kind: "semantic_pending",
        threadOutRef: outRef(utxo),
        semanticResolverGlobalIndex: globalIndex,
      };
    });
  });
  const groups: readonly (readonly [
    ValidationTraceDisputeSemanticGroup,
    unknown,
  ])[] = [
    ["cek_material_traversal", chain.cekMaterialTraversal],
    ["cek_core_stage", chain.cekCoreStages],
    ["cek_context_stage", chain.cekContextStages],
    ["cek_context_item_stage", chain.cekContextItemStages],
    ["canonical_decode_item_stage", chain.canonicalDecodeItemStages],
    ["script_sources_item_stage", chain.scriptSourcesStageOneRedeemerStages],
    ["proof_item", chain.proofItem],
  ];
  for (const [group, value] of groups) {
    groupAddresses(
      value,
      (address, role) => {
        put(address, (utxo) =>
          inFlight(group, role === "" ? group : role)(utxo),
        );
      },
      "",
    );
  }
  return map;
};

/**
 * Lucid providers signal an absent unit from `getUtxoByUnit` by throwing
 * "Unit not found." (Blockfrost, Kupmios); the emulator resolves
 * `undefined` instead. The error can reach callers wrapped (an Effect
 * FiberFailure, or a provider error carrying it as `cause`), so the check
 * reads the whole cause chain.
 */
const isUnitNotFoundError = (error: unknown): boolean =>
  /\bUnit not found\b/u.test(formatUnknownError(error, { includeCause: true }));

/**
 * Derives the R2 dispute cursor from live chain state only: the unique
 * computation-thread token (looked up by its deterministic unit, wherever
 * the interactive chain has carried it), the permanent proof token, and the
 * fraudulent state-queue header's presence.
 */
export const deriveValidationTraceDisputeChainStage = async ({
  lucid,
  chain,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofSpendingScriptAddress,
  stateQueue,
  categoryId,
  headerHash,
  currentTime,
}: {
  readonly lucid: LucidEvolution;
  readonly chain: FamilyChain;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofSpendingScriptAddress: string;
  readonly stateQueue: Readonly<{ policyId: string; address: string }>;
  readonly categoryId: string;
  readonly headerHash: string;
  readonly currentTime: number;
}): Promise<ValidationTraceDisputeChainStage> => {
  if (
    !/^[0-9a-f]{8}$/u.test(categoryId) ||
    !/^[0-9a-f]{56}$/u.test(headerHash)
  ) {
    throw new Error(
      "validationTraceDispute cursor requires canonical category and header bytes",
    );
  }
  const assetName = `${categoryId}${headerHash}`;
  const threadUnit = toUnit(computationThreadPolicyId, assetName);
  const proofUnit = toUnit(fraudProofPolicyId, assetName);
  const classifier = buildValidationTraceDisputeAddressClassifier(chain);
  const thread: UTxO | undefined = await lucid
    .utxoByUnit(threadUnit)
    .catch((error: unknown) => {
      // Only a provider's explicit "Unit not found." means no live thread;
      // any other failure (network, HTTP status, non-NFT unit) fails closed
      // rather than being misread as `not_started`.
      if (isUnitNotFoundError(error)) return undefined;
      throw error;
    });
  if (thread !== undefined) {
    const entry = classifier.get(thread.address);
    if (entry === undefined) {
      throw new Error(
        `validationTraceDispute thread token surfaced at unknown address ${thread.address}`,
      );
    }
    const stage = entry.classify(thread, currentTime);
    if (stage.kind !== "init") return stage;
    const topology = await stateQueueHeaderTopology({
      lucid,
      stateQueue,
      headerHash,
    });
    if (topology.target === undefined) {
      throw new Error(
        "validationTraceDispute thread is open but the fraudulent header left the state queue",
      );
    }
    return { ...stage, stateQueueBlockOutRef: topology.target };
  }
  const topology = await stateQueueHeaderTopology({
    lucid,
    stateQueue,
    headerHash,
  });
  const proof = (await lucid.utxosAt(fraudProofSpendingScriptAddress)).find(
    (utxo) => (utxo.assets[proofUnit] ?? 0n) === 1n,
  );
  if (topology.target === undefined) {
    if (proof === undefined) {
      throw new Error(
        "validationTraceDispute fraudulent header disappeared without a retained proof token",
      );
    }
    return { kind: "removed" };
  }
  if (proof !== undefined) {
    return {
      kind: "proof_token",
      fraudProofOutRef: outRef(proof),
      nextRemovalOutRef: topology.successor ?? topology.target,
    };
  }
  return { kind: "not_started", stateQueueBlockOutRef: topology.target };
};

/**
 * Authenticated fraudulent-header lookup over the live state queue: the
 * node NFT's asset name, linked-list key, and recomputed header hash must
 * all agree before an out-ref is trusted (the same discipline as the shared
 * raw-L1 derivation).
 */
const stateQueueHeaderTopology = async ({
  lucid,
  stateQueue,
  headerHash,
}: {
  readonly lucid: LucidEvolution;
  readonly stateQueue: Readonly<{ policyId: string; address: string }>;
  readonly headerHash: string;
}): Promise<{
  readonly target: string | undefined;
  readonly successor: string | undefined;
}> => {
  const candidates = (await lucid.utxosAt(stateQueue.address)).filter((utxo) =>
    Object.entries(utxo.assets).some(
      ([unit, quantity]) =>
        unit.startsWith(stateQueue.policyId) && quantity !== 0n,
    ),
  );
  const decoded = await Promise.all(
    candidates.map((utxo) =>
      Effect.runPromise(utxoToStateQueueUTxO(utxo, stateQueue.policyId)),
    ),
  );
  const ordered = await Effect.runPromise(sortStateQueueUTxOs(decoded));
  const hashes = await Promise.all(
    ordered.map(async (node) => {
      if (node.datum.key === "Empty") return null;
      const assetHash = node.assetName.slice(
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
      );
      if (
        !node.assetName.startsWith(STATE_QUEUE_NODE_ASSET_NAME_PREFIX) ||
        node.datum.key.Key.key !== assetHash
      ) {
        throw new Error(
          "validationTraceDispute state-queue node token and linked-list key disagree",
        );
      }
      const header = await Effect.runPromise(
        getHeaderFromStateQueueDatum(node.datum),
      );
      if ((await Effect.runPromise(hashBlockHeader(header))) !== assetHash) {
        throw new Error(
          "validationTraceDispute state-queue node datum and authentication token disagree",
        );
      }
      return assetHash;
    }),
  );
  const targetIndex = hashes.indexOf(headerHash);
  return {
    target: targetIndex < 0 ? undefined : outRef(ordered[targetIndex]!.utxo),
    successor:
      targetIndex < 0 || targetIndex + 1 >= ordered.length
        ? undefined
        : outRef(ordered[targetIndex + 1]!.utxo),
  };
};
