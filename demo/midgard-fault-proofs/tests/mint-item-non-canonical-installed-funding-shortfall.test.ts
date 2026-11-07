/**
 * A mint-item proof the prover wallet cannot pay for, through the watcher's
 * installed application: the family runs its own journaled workflow, not the
 * shared orchestrator, and pauses the same way. A wallet that cannot pay for
 * the next transaction pauses the objective with the funding-shortfall
 * reason and releases its reservation; a top-up resumes it to conviction. A
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
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { OutRef } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
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
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import {
  MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS,
  MintItemStep02DatumSchema,
  MintItemStep03DatumSchema,
  MintItemStep04DatumSchema,
} from "../src/mint-item-non-canonical/index.js";
import { MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { WORKFLOW_FUNDING_SHORTFALL } from "../src/workflow/funding-reservation-permit.js";
import {
  drainProver,
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
/** Confirmation polls a run may take before it counts as spinning. */
const POLL_CAP = 5_000;

type Harness = InstalledTurnContext["harness"];

describe("a mint-item proof the prover wallet cannot fund, in the installed watcher application", () => {
  it("pauses on a wallet whose ada sits in a coin holding a token, and convicts after a top-up without spending that coin", async () => {
    let tokenCoin: OutRef | undefined;
    let harness: Harness | undefined;
    let toppedUp = false;
    const pausedReasons: string[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: async (context) => {
        harness = context.harness;
        context.clock.state.crashAfterPolls = POLL_CAP;
        await drainProver(context.harness, 1_500_000n);
        tokenCoin = await sendTokenCoin(context.harness, 5_000n * ADA);
      },
      afterTurn: async ({ harness, result }) => {
        if (toppedUp) return;
        if (result.reason?.startsWith(WORKFLOW_FUNDING_SHORTFALL) !== true)
          return;
        // Paused, not wedged: it pauses again on the next observation.
        pausedReasons.push(result.reason);
        if (pausedReasons.length < 2) return;
        await topUpProver(harness, 5_000n * ADA);
        toppedUp = true;
      },
    });

    expect(pausedReasons).toHaveLength(2);
    expect(run.convicted).toBe(true);
    expect(await harness!.emulator.getUtxosByOutRef([tokenCoin!])).toHaveLength(
      1,
    );
  }, 1_800_000);
});
