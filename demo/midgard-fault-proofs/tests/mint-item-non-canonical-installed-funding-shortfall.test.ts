/**
 * A mint-item proof the prover wallet cannot pay for, through the watcher's
 * installed application: the family runs its own workflow, not the shared
 * orchestrator, and pauses the same way. A wallet that cannot pay for the
 * next transaction pauses the objective with the funding-shortfall reason; a
 * top-up resumes it to conviction. A
 * coin holding a token is never spent.
 */
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";
import type { OutRef } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import {
  MintItemStep02DatumSchema,
  MintItemStep03DatumSchema,
  MintItemStep04DatumSchema,
} from "../src/mint-item-non-canonical/schemas.js";
import { MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS } from "../src/mint-item-non-canonical/workflow.load-manifest-bound-mint-item-non-canonical-config.js";
import { MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { WORKFLOW_FUNDING_SHORTFALL } from "../src/workflow/workflow-funding-shortfall.js";
import {
  drainProverToOneCoin,
  sendTokenCoin,
  topUpProver,
} from "./support/installed-prover-wallet.js";
import { mintField } from "./support/mint-item-vectors.js";
import {
  type InstalledTurnContext,
  runInstalled,
} from "./support/script-decode-installed-run.js";

const names = MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS;

/** A transaction whose only mint item names the ada policy. */
const nativeTx = () =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: mintField(Buffer.from("8240a14005", "hex")),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 1_009n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

/** The mint-item family as the installed application binds it. */
const SCENARIO = {
  definition: {
    category: "mintItemNonCanonical",
    adapter: {
      kind: "cursor",
      stepContractNames: [
        names.step01,
        names.step02,
        names.step03,
        names.step04,
      ],
    },
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MintItemStep02DatumSchema,
      MintItemStep03DatumSchema,
      MintItemStep04DatumSchema,
    ],
    witnessRoles: [
      names.computationThreadMint,
      names.fraudProofMint,
      names.phasMembershipWithdraw,
    ],
  } as never,
  replayer: MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  contractOptions: {
    realMintItemNonCanonical: true,
    alwaysFraudProofCatalogue: true,
  },
  nativeTx: nativeTx as never,
  violationId: "mint-item-non-canonical",
};
const ADA = 1_000_000n;

type Harness = InstalledTurnContext["harness"];

describe("a mint-item proof the prover wallet cannot fund, in the installed watcher application", () => {
  it("pauses on a wallet whose ada sits in a coin holding a token, and convicts after a top-up without spending that coin", async () => {
    let drained = false;
    let tokenCoin: OutRef | undefined;
    let harness: Harness | undefined;
    let toppedUp = false;
    const pausedReasons: string[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: (context) => {
        harness = context.harness;
        // Once the proof has begun, the operator leaves one plain coin below
        // the collateral line between two of its actions; the
        // wallet's ada now sits in a coin that also holds a token.
        context.clock.state.onObservation = async () => {
          if (drained) return;
          drained = true;
          await drainProverToOneCoin(context.harness, 1_500_000n);
          tokenCoin = await sendTokenCoin(context.harness, 5_000n * ADA);
        };
      },
      afterTurn: async ({ harness, result }) => {
        if (!drained || toppedUp) return;
        if (result.reason?.startsWith(WORKFLOW_FUNDING_SHORTFALL) !== true)
          return;
        // Paused, not wedged: it pauses again on the next observation.
        pausedReasons.push(result.reason);
        if (pausedReasons.length < 2) return;
        await topUpProver(harness, 5_000n * ADA);
        toppedUp = true;
      },
    });

    expect(drained).toBe(true);
    expect(pausedReasons).toHaveLength(2);
    expect(run.convicted).toBe(true);
    expect(await harness!.emulator.getUtxosByOutRef([tokenCoin!])).toHaveLength(
      1,
    );
  }, 1_800_000);
});
