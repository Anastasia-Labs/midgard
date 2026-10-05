import fs from "node:fs";
import path from "node:path";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  type LocalScriptEvalResult,
  plutusDataToCborHex,
  txOutRefData,
} from "@al-ft/midgard-validation";
import { encodeMidgardCekPlutusData } from "@al-ft/midgard-validation/cek-constant";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "@al-ft/midgard-validation/cek-executor";
import { buildMidgardCanonicalCekProgram } from "@al-ft/midgard-validation/cek-program";
import type { MidgardLedgerRedeemer } from "@al-ft/midgard-validation/ledger-tx/types";
import { CML, Constr, Data } from "@lucid-evolution/lucid";

import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprintPath = path.resolve(
  __dirname,
  "../blueprints/always-succeeds/plutus.json",
);

const alwaysSucceedsBlueprint = JSON.parse(
  fs.readFileSync(alwaysSucceedsBlueprintPath, "utf8"),
) as {
  readonly validators: readonly BlueprintValidator[];
};

const loadAlwaysSucceedsCompiledCode = (title: string): string => {
  const compiledCode = alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === title,
  )?.compiledCode;
  if (compiledCode === undefined) {
    throw new Error(`missing always-succeeds blueprint entry: ${title}`);
  }
  return compiledCode;
};

export const MIDGARD_SPEND_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_spend_guard.else",
);

export const MIDGARD_SPEND_OUT_REF_GUARD_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.midgard_v1_spend_out_ref_guard.else");

export const MIDGARD_RECEIVE_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_receive_guard.else",
);

export const MIDGARD_ALWAYS_FAIL_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_always_fail.else",
);

export const MIDGARD_CONTEXT_PROBE_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_context_probe.else",
);

export const txOutRefDataCborHex = (outRefHex: string): string =>
  plutusDataToCborHex(txOutRefData(outRefHex));

export const ledgerRedeemer = <
  T extends Omit<MidgardLedgerRedeemer, "dataCbor"> & {
    readonly dataCborHex: string;
  },
>(
  redeemer: T,
): T & MidgardLedgerRedeemer => ({
  ...redeemer,
  dataCbor: Buffer.from(redeemer.dataCborHex, "hex"),
});

/** The context as Lucid Data, for assertions on map-free parts of it. */
export const lucidContext = (
  context: Parameters<typeof encodeMidgardCekPlutusData>[0],
): Constr<unknown> =>
  Data.from(
    Buffer.from(encodeMidgardCekPlutusData(context)).toString("hex"),
  ) as Constr<unknown>;

/** Runs a compiled script on a context through the structural CEK executor. */
export const evaluateScriptStructurally = (
  scriptBytes: Uint8Array,
  context: Parameters<typeof encodeMidgardCekPlutusData>[0],
): LocalScriptEvalResult => {
  const program = buildMidgardCanonicalCekProgram(scriptBytes);
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
    program.material.values(),
    encodeMidgardCekPlutusData(context),
  );
  const cek = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
  });
  return cek.terminalState.mode === "haltSuccess"
    ? {
        kind: "accepted",
        budget: {
          cpu: cek.terminalState.cpu,
          memory: cek.terminalState.memory,
        },
      }
    : {
        kind: "script_invalid",
        detail: `V1 CEK halted with error ${cek.terminalState.auxiliary.toString(10)}`,
      };
};

export const makeOutRefHex = (
  txHashByte: number,
  outputIndex: bigint,
): string => makeOutRefCbor(txHashByte, outputIndex).toString("hex");

export const makeScriptContextOutput = (
  scriptHash: string,
): ReturnType<typeof makeMidgardTxOutput> =>
  makeMidgardTxOutput(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
    ).to_address(),
    CML.Value.from_coin(2_000_000n),
  );

export const makeMidgardContextProbeRedeemerCborHex = (opts: {
  readonly expectedSpendScriptHash: string;
  readonly expectedOwnRef: string;
  readonly expectedFirstInput: string;
  readonly expectedSecondInput: string;
  readonly expectedFirstReference: string;
  readonly expectedSecondReference: string;
  readonly expectedFirstOutputScriptHash: string;
  readonly expectedSecondOutputScriptHash: string;
  readonly expectedSigner: string;
  readonly expectedObserver: string;
  readonly expectedPolicy: string;
  readonly expectedAssetName: string;
  readonly expectedMintQuantity: bigint;
  readonly expectedMintRedeemer: unknown;
  readonly expectedObserveRedeemer: unknown;
  readonly expectedReceiveScriptHash: string;
  readonly expectedReceiveRedeemer: unknown;
}): string =>
  plutusDataToCborHex(
    new Constr(0, [
      opts.expectedSpendScriptHash,
      txOutRefData(opts.expectedOwnRef),
      txOutRefData(opts.expectedFirstInput),
      txOutRefData(opts.expectedSecondInput),
      txOutRefData(opts.expectedFirstReference),
      txOutRefData(opts.expectedSecondReference),
      opts.expectedFirstOutputScriptHash,
      opts.expectedSecondOutputScriptHash,
      opts.expectedSigner,
      opts.expectedObserver,
      opts.expectedPolicy,
      opts.expectedAssetName,
      opts.expectedMintQuantity,
      opts.expectedMintRedeemer,
      opts.expectedObserveRedeemer,
      opts.expectedReceiveScriptHash,
      opts.expectedReceiveRedeemer,
    ]),
  );
