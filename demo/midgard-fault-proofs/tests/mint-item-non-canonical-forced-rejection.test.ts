/**
 * A forced `MintItemMalformed` rejection is wrongful: no machine step emits
 * that reason. Step 01 opens the forced leaf and writes the terminal state
 * without opening the mint field, step 04 mints the proof and the block is
 * removed. Step 01 refuses the route for a leaf that rejects with any other
 * reason and for an accepted leaf, and the route convicts through the
 * deepest forced-transactions membership proof.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  type RejectionReason,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitMintItemNonCanonicalStep01Forced,
  submitMintItemNonCanonicalStep04UnreachableClaim,
} from "../src/mint-item-non-canonical/index.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { submitMintStep01ForcedUnreachableRaw } from "./support/mint-item-non-canonical-emulator.js";
import { registeredContracts } from "./support/mint-item-non-canonical-lifecycle.js";
import { mintField } from "./support/mint-item-vectors.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  MAXIMUM_PROOF_STEP_COUNT,
  syntheticDeepMembershipProof,
} from "./support/synthetic-deep-proof.js";

const MINT_ITEM_MALFORMED: RejectionReason = {
  MintItemMalformed: { policy_index: 0n },
};

/**
 * A block committing one forced leaf with `verdict` (an accepted leaf when
 * `reason` is null), the thread's Init, and the family's references.
 */
const forcedThread = async ({
  reason,
  proofDepth = 0,
}: {
  readonly reason: RejectionReason | null;
  readonly proofDepth?: number;
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMintItemNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const registered = await registeredContracts(harness);
  const tx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    mintPreimageCbor: mintField(Buffer.from("8240a14005", "hex")),
  });
  const id = computeMidgardNativeTxId(tx).toString("hex");
  const source = deriveMidgardForcedTxProofSource(tx);
  const key = { transactionId: "ab".repeat(32), outputIndex: 0n };
  const value = {
    tx_id: id,
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict:
      reason === null
        ? ("ForcedTxValid" as const)
        : { ForcedTxInvalid: { reason } },
  };
  const bytes = {
    key: Buffer.from(Data.to(key, OutputReference), "hex"),
    value: Buffer.from(
      Data.to(value as never, ForcedInclusionTxV1Schema as never),
      "hex",
    ),
  };
  let root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [bytes]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(bytes.key, bytes.value);
  const deep =
    proofDepth > 0
      ? syntheticDeepMembershipProof({
          key: bytes.key,
          value: bytes.value,
          branchLevels: proofDepth,
        })
      : undefined;
  if (deep !== undefined)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
      root: await commitCountedRoot({
        domain: root.domain,
        phasRoot: deep.transactionsPhasRoot,
        count: root.count,
      }),
    };
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key,
    value,
    proof: Data.from(
      deep?.proofCbor ?? (await trie.prove(bytes.key)).toCBOR().toString("hex"),
      Proof,
    ),
  };
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("funder key absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header: {
      ...base.header,
      forcedTransactionsRoot: root.root,
      forcedTransactionCount: root.count,
    },
  });
  await registered.publishReferences();
  const init = await registered.init(setup.fraudulentBlockOutRef);
  return {
    harness,
    registered,
    setup,
    forcedSource: { header: setup.header, membership },
    subject: forcedVerdictSubject({
      transactionId: id,
      sourceKey: key,
      rejectionReason: reason,
    }),
    threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex}`,
  };
};

/** Step 01 to the terminal, step 04 and the block's removal. */
const convict = async ({
  harness,
  registered,
  setup,
  forcedSource,
  subject,
  threadOutRef,
}: Awaited<ReturnType<typeof forcedThread>>) => {
  const bound = await captureEmulatorSubmission(harness.emulator, () =>
    submitMintItemNonCanonicalStep01Forced({
      ...registered.common(threadOutRef, 0),
      finding: { subject, fieldIndex: 5, itemIndex: 0 },
      forcedSource,
    }),
  );
  const minted = await captureEmulatorSubmission(harness.emulator, () =>
    submitMintItemNonCanonicalStep04UnreachableClaim({
      ...registered.common(bound.result.nextThreadOutRef, 3),
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  expect(minted.result.fraudProofUnit).toBeTruthy();
  await registered.removal(setup.headerHash);
  expect(
    await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueBlockUnit,
    ),
  ).toHaveLength(0);
  return { bound, minted };
};

/** Leaves the terminal route does not apply to. */
const HONEST_LEAVES: readonly {
  readonly leaf: string;
  readonly reason: RejectionReason | null;
  readonly check: RegExp;
}[] = [
  {
    leaf: "a rejection with another reason",
    reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
    check:
      /^expect Some\(MintItemMalformed \{ \.\. \}\) = subject\.rejection_reason/u,
  },
  {
    leaf: "an accepted leaf",
    reason: null,
    check: /^Validator returned false$/u,
  },
];

describe("mintItemNonCanonical forced MintItemMalformed rejection", () => {
  it("convicts a forced rejection with MintItemMalformed", async () => {
    await convict(await forcedThread({ reason: MINT_ITEM_MALFORMED }));
  }, 600_000);

  it.each(HONEST_LEAVES)(
    "refuses on chain the terminal route for $leaf",
    async ({ reason, check }) => {
      const thread = await forcedThread({ reason });
      await expectOnchainRefusal(
        () =>
          submitMintStep01ForcedUnreachableRaw({
            ...thread.registered.common(thread.threadOutRef, 0),
            subject: thread.subject,
            forcedSource: thread.forcedSource,
          }),
        {
          refusedBy: "fraud_proofs/mint_item_non_canonical/step_01",
          check,
        },
      );
    },
    600_000,
  );

  it("convicts through the deepest forced-transactions membership proof", async () => {
    const { bound, minted } = await convict(
      await forcedThread({
        reason: MINT_ITEM_MALFORMED,
        proofDepth: MAXIMUM_PROOF_STEP_COUNT,
      }),
    );
    for (const { measurement } of [bound, minted]) {
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_MEMORY_UNITS,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_CPU_UNITS,
      );
    }
    console.info(
      `max-shape unreachable claim (proof depth ${MAXIMUM_PROOF_STEP_COUNT.toString()}): step-01 mem=${bound.measurement.executionMemory.toString()} cpu=${bound.measurement.executionSteps.toString()}; step-04 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
    );
  }, 600_000);
});
