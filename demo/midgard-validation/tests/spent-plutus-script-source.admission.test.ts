import {
  encodeCbor,
  ledgerEd25519SignatureReady,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
} from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  MidgardRedeemerTag,
  RejectCodes,
  runPhaseBValidationWithPatch,
  validatePhaseASingle,
} from "../src/index.js";
import type { QueuedTx } from "../src/types.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeQueued,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

/**
 * Phase B looks a purpose's script up among the reference scripts of every
 * resolved output, spent outputs included, as the Cardano ledger does. The
 * carried script bytes are the whole program, so a non-native script whose
 * only source is a spent output's reference script runs like any other. A
 * forced transaction carries no redeemer, so it runs native scripts only.
 */

const script = plutusV3ScriptWitness(
  encodeCbor(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(UPLCConst.unit)),
      ),
    ),
  ),
);

const lockedBy = (scriptRef?: MidgardVersionedScript) =>
  encodeMidgardTxOutput({
    address: protectMidgardAddress(
      Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_script(
            CML.ScriptHash.from_hex(hashScriptWitness(script)),
          ),
        )
          .to_address()
          .to_raw_bytes(),
      ),
    ),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    ...(scriptRef === undefined ? {} : { script_ref: scriptRef }),
  });

const carrying = encodeMidgardTxOutput({
  address: TEST_ADDRESS_BYTES,
  value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  script_ref: script,
});

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

const [spent, reference] = [outRefFromByte(0x81), outRefFromByte(0x82)];
const SLOT = 200_000n;

type Spelling = {
  readonly referenceInputs: readonly Buffer[];
  readonly inline: boolean;
  readonly sourceKind?: "forced";
};

const transaction = (spelling: Spelling) =>
  makeNativeTx({
    spendInputs: [spent],
    referenceInputs: [...spelling.referenceInputs],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    validityIntervalStart: SLOT,
    ...(spelling.inline ? { scriptWitnesses: [script] } : {}),
    ...(spelling.sourceKind === "forced"
      ? {}
      : {
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            {
              tag: MidgardRedeemerTag.Spend,
              index: 0n,
              exUnits: [10_000_000n, 10_000_000_000n],
            },
          ]),
          scriptLanguages: ["PlutusV3"],
        }),
  });

/** Phase A, then Phase B over `state`: the node's admission verdict. */
const admit = async (
  spelling: Spelling,
  state: readonly (readonly [Buffer, Buffer])[],
) => {
  const fixture = transaction(spelling);
  const txCbor =
    spelling.sourceKind === "forced"
      ? encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(fixture.tx),
        )
      : fixture.txCbor;
  const queued: QueuedTx = {
    ...makeQueued(fixture.txId, txCbor),
    ...(spelling.sourceKind === undefined
      ? {}
      : { sourceKind: spelling.sourceKind }),
  };
  const phaseA = validatePhaseASingle(queued, phaseAConfig);
  if (!("ledgerTx" in phaseA)) return { phase: "A" as const, verdict: phaseA };
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [phaseA],
      new Map(state.map(([key, output]) => [key.toString("hex"), output])),
      {
        nowCardanoSlotNo: SLOT + 100n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );
  return {
    phase: "B" as const,
    verdict: phaseB.rejected[0] ?? { accepted: phaseB.accepted.length },
  };
};

describe("a non-native script whose only source is a spent output", () => {
  it("is admitted when the spent output carries the script", async () => {
    const { phase, verdict } = await admit(
      { referenceInputs: [], inline: false },
      [[spent, lockedBy(script)]],
    );
    expect({ phase, verdict }).toStrictEqual({
      phase: "B",
      verdict: { accepted: 1 },
    });
  });

  it("is admitted with an unrelated reference input", async () => {
    const { verdict } = await admit(
      { referenceInputs: [reference], inline: false },
      [
        [spent, lockedBy(script)],
        [reference, makeOutput(FUNDED_OUTPUT_LOVELACE)],
      ],
    );
    expect(verdict).toStrictEqual({ accepted: 1 });
  });

  it("is admitted when the same program is a reference input's script", async () => {
    const { verdict } = await admit(
      { referenceInputs: [reference], inline: false },
      [
        [spent, lockedBy()],
        [reference, carrying],
      ],
    );
    expect(verdict).toStrictEqual({ accepted: 1 });
  });

  it("is admitted when the same program is an inline witness", async () => {
    const { verdict } = await admit({ referenceInputs: [], inline: true }, [
      [spent, lockedBy()],
    ]);
    expect(verdict).toStrictEqual({ accepted: 1 });
  });
});

describe("a forced transaction runs native scripts only", () => {
  it("refuses a forced transaction carrying a redeemer before it is ordered", () => {
    const { tx } = transaction({
      referenceInputs: [],
      inline: false,
    });
    expect(screenMidgardForcedTxAdmission(tx)).toMatchObject({
      check: "has_no_redeemers(witness_set)",
    });
  });

  it("refuses a forced spend of a non-native script output", async () => {
    const { phase, verdict } = await admit(
      {
        referenceInputs: [],
        inline: false,
        sourceKind: "forced",
      },
      [[spent, lockedBy(script)]],
    );
    expect({ phase, verdict }).toMatchObject({
      phase: "B",
      verdict: {
        code: RejectCodes.MissingRequiredWitness,
        subject: { arm: "RedeemerMissing", purposeKind: 0n, purposeIndex: 0n },
      },
    });
  });
});
