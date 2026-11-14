import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { readCborHexFile, readJsonFile } from "./json-file.js";
import {
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  parseOutRef,
  type ProverSignerConfig,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { type TransitionL2ReplayMaterial } from "./transition-trace/proof-material.js";
import {
  submitTransitionTraceProof,
  type SubmitTransitionTraceProofResult,
} from "./transition-trace/submit.js";

const L2_REPLAY_KEYS = [
  "spendInputCbors",
  "outputCbors",
  "spentUtxos",
  "producedUtxos",
] as const;

/**
 * Read `--transition-l2-replay`: a JSON object whose four fields are arrays of
 * CBOR hex — the spend-input and output items, and the canonical Plutus Data
 * of each `LedgerDeleteWitness` and `LedgerInsertWitness`, in replay order.
 */
export const readTransitionL2ReplayFile = async (
  path: string,
): Promise<TransitionL2ReplayMaterial> => {
  const label = "--transition-l2-replay";
  const parsed = await readJsonFile(path);
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    Array.isArray(parsed) ||
    Object.keys(parsed).sort().join(",") !==
      [...L2_REPLAY_KEYS].sort().join(",")
  )
    throw new Error(
      `${label} must contain exactly ${L2_REPLAY_KEYS.join(", ")}`,
    );
  const field = (key: (typeof L2_REPLAY_KEYS)[number]): readonly string[] => {
    const items = (parsed as Record<string, unknown>)[key];
    if (
      !Array.isArray(items) ||
      !items.every(
        (item): item is string =>
          typeof item === "string" && /^(?:[0-9a-f]{2})+$/u.test(item),
      )
    )
      throw new Error(`${label} ${key} must be an array of CBOR hex`);
    return items;
  };
  const canonical = (cbor: string, encoded: string) => {
    if (encoded !== cbor)
      throw new Error(`${label} ledger witnesses must be canonical`);
  };
  return Object.freeze({
    spendInputCbors: field("spendInputCbors"),
    outputCbors: field("outputCbors"),
    spentUtxos: field("spentUtxos").map((cbor) => {
      const witness = Data.from(cbor, SDK.LedgerDeleteWitness);
      canonical(cbor, Data.to(witness, SDK.LedgerDeleteWitness));
      return witness;
    }),
    producedUtxos: field("producedUtxos").map((cbor) => {
      const witness = Data.from(cbor, SDK.LedgerInsertWitness);
      canonical(cbor, Data.to(witness, SDK.LedgerInsertWitness));
      return witness;
    }),
  });
};

export type SubmitTransitionTraceProofFromCborFileConfig =
  SubmitProviderConfig &
    ProverSignerConfig & {
      readonly blueprintPath: string;
      readonly deploymentInfoPath: string;
      readonly threadOutRef: string;
      readonly transitionFaultProofPath: string;
      /** The accepted-transaction replay material a kind-0 proof needs. */
      readonly transitionL2ReplayPath?: string;
      readonly referenceInputOutRefs?: readonly string[];
      readonly awaitConfirmation?: boolean;
    };

/** Decode a strict TransitionFaultProof file and submit its route/final pair. */
export const submitTransitionTraceProofFromCborFile = async (
  config: SubmitTransitionTraceProofFromCborFileConfig,
): Promise<SubmitTransitionTraceProofResult> => {
  const proofCbor = await readCborHexFile(
    config.transitionFaultProofPath,
    "--transition-fault-proof",
  );
  Data.from(proofCbor, SDK.TransitionFaultProof);
  if (
    aikenSerialisedPlutusDataCborPreservingMapOrder(proofCbor) !== proofCbor
  ) {
    throw new Error(
      "--transition-fault-proof must contain canonical TransitionFaultProof CBOR",
    );
  }

  const referenceInputOutRefs = config.referenceInputOutRefs ?? [];
  if (new Set(referenceInputOutRefs).size !== referenceInputOutRefs.length) {
    throw new Error("--reference-input values must be unique");
  }

  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const additionalReferenceInputs = await Promise.all(
    referenceInputOutRefs.map((outRef, index) =>
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(outRef, `--reference-input[${index.toString()}]`),
        label: `transition-trace reference input ${index.toString()}`,
      }),
    ),
  );

  return await submitTransitionTraceProof({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer: resolveProverSigner(config),
    threadOutRef: config.threadOutRef,
    proof: Object.freeze({
      proofCbor,
      ...(config.transitionL2ReplayPath === undefined
        ? {}
        : {
            l2Replay: await readTransitionL2ReplayFile(
              config.transitionL2ReplayPath,
            ),
          }),
    }),
    additionalReferenceInputs,
    awaitConfirmation: config.awaitConfirmation,
  });
};
