import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { MIDGARD_COINS_PER_UTXO_BYTE } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { admitMinAdaForcedArtifact } from "../src/min-ada/forced-artifact.js";
import { submitMinAdaStep01Forced } from "../src/min-ada/submit-step-01-forced.js";
import { submitMinAdaTxStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaUtxoStep03 } from "../src/min-ada/submit-step-03.js";
import { submitMinAdaStep05 } from "../src/min-ada/submit-step-05.js";
import { admitMinAdaWorkflowArtifact } from "../src/min-ada/workflow-artifact.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { setup } from "./min-ada-wrongful-rejection-lifecycle.setup.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  signedKeyOutput,
  signNativeTx,
} from "./support/forced-reason-signed-native-tx.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

/**
 * A forced OutputBelowMinAda reason names an output by its field position,
 * and minAda reopens exactly that output item. The transaction pays a funded
 * output, then one a lovelace under its floor, from a signed input, so the
 * node reaches the minimum-Ada rule. The verdict is the one the node's
 * classifier writes, so the suite fails if the writer and the proof disagree
 * on how outputs are counted: one position early names the funded output and
 * convicts; the written position is refused on chain.
 */

const WRITTEN_OUTPUT_INDEX = 1;

const input = encodeMidgardSpendInputItem({
  txId: Buffer.alloc(32, 0x11),
  outputIndex: 0,
});

const ledger = [[input, signedKeyOutput(10_000_000n)]] as const;

/** A block whose forced leaf names output `written + offset`. */
const setupScenario = async (offset: number) => {
  const f = await setup({
    underfunded: true,
    prefixCount: 1,
    namedOutputIndex: WRITTEN_OUTPUT_INDEX + offset,
    transactionOf: (outputs) =>
      signNativeTx(
        makeNativeTx({
          spendInputCbors: [input],
          fee: 7n,
          outputCbors: [...outputs],
        }),
      ),
  });
  expect(
    await nodeForcedVerdict({
      transactionId: Buffer.from(f.prepared.badTxId, "hex"),
      forcedCanonicalCbor: Buffer.from(f.artifact.fullTransactionCbor, "hex"),
      ledger,
    }),
  ).toStrictEqual({
    ForcedTxInvalid: {
      reason: {
        OutputBelowMinAda: { output_index: BigInt(WRITTEN_OUTPUT_INDEX) },
      },
    },
  });
  return f;
};

describe("forced OutputBelowMinAda coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the output is funded", async () => {
    const f = await setupScenario(-1);
    const workflowArtifact = await f.prepareArtifact();
    await admitMinAdaWorkflowArtifact(
      JSON.parse(JSON.stringify(workflowArtifact)),
    );
    const reopened = await admitMinAdaForcedArtifact(
      JSON.parse(JSON.stringify(workflowArtifact)),
    );
    const bound = await submitMinAdaStep01Forced({
      ...f.common,
      threadOutRef: f.threadOutRef,
      state: reopened.evidence.state,
      forcedSource: reopened.forcedSource,
      referenceScriptUtxo: f.refs[0]!,
    });
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: 1n,
      fieldIndex: 2,
      anchorTxId: f.prepared.badTxId,
      nativeTxCompactCbor: f.prepared.nativeTxCompactCbor,
      itemCbors: f.prepared.outputItemCbors.map((item) =>
        Buffer.from(item, "hex"),
      ),
      owner: f.h.proverSigner.paymentKeyHash,
      publish: true,
      label: "min-ada outputs",
    });
    // Two small outputs open without a certificate.
    expect(planned.plan.tier).not.toBe("Certified");
    const carriage = await publishFaultProofFieldCarriage({
      lucid: f.h.proverLucid,
      signer: f.h.proverSigner,
      planned,
      publisherAddress: f.h.proverSigner.address,
      label: "min-ada outputs",
    });
    const opened = await submitMinAdaTxStep02({
      ...f.common,
      threadOutRef: bound.nextThreadOutRef,
      prepared: f.prepared,
      publishedCarriageUtxos: carriage,
      referenceScriptUtxo: f.refs[1]!,
      yieldReferenceScriptUtxo: f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
    });
    const checked = await submitMinAdaUtxoStep03({
      ...f.common,
      threadOutRef: opened.nextThreadOutRef,
      outputItemCbors: f.prepared.outputItemCbors,
      coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
      referenceScriptUtxo: f.refs[2]!,
    });
    const proof = await submitMinAdaStep05({
      ...f.common,
      threadOutRef: checked.nextThreadOutRef,
      referenceScriptUtxo: f.refs[4]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    });
    expect(proof.fraudProofUnit).toBeTruthy();
    const removal = await publishRemovalReferenceScripts({
      lucid: f.h.proverLucid,
      contracts: f.h.contracts,
    });
    const now = BigInt(f.h.emulator.now());
    await submitRemoveFraudulentBlock({
      lucid: f.h.proverLucid,
      blueprint: f.h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(f.h.contracts, f.h.catalogue, {
        removalReferenceScripts: removal.published,
      }),
      network,
      signer: f.h.proverSigner,
      fraudCategory: "minAda",
      fraudulentHeaderHash: f.seeded.headerHash,
      requireReferenceScripts: true,
      validFrom: now > 120000n ? now - 120000n : 0n,
      validTo: now + 300000n,
    });
    expect(
      await f.h.proverLucid.utxosAtWithUnit(
        f.h.contracts.stateQueue.spendingScriptAddress,
        f.seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(0);
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const f = await setupScenario(0);
    // The written output is the underfunded one: the rejection holds, and
    // neither the replay nor the artifact gate finds a contradiction.
    await expect(f.prepareArtifact()).rejects.toThrow(
      /installed complete replay missed forced minAda/u,
    );
    await expect(admitMinAdaForcedArtifact(f.artifact)).rejects.toThrow(
      /no contradiction/u,
    );
    const bound = await submitMinAdaStep01Forced({
      ...f.common,
      threadOutRef: f.threadOutRef,
      state: f.state,
      forcedSource: f.source,
      referenceScriptUtxo: f.refs[0]!,
    });
    const opened = await submitMinAdaTxStep02({
      ...f.common,
      threadOutRef: bound.nextThreadOutRef,
      prepared: f.prepared,
      referenceScriptUtxo: f.refs[1]!,
      yieldReferenceScriptUtxo: f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
      unsafeSkipLocalViolationCheckForTest: true,
    });
    // The predicate reopens the named output and, on the forced direction,
    // returns whether it meets its floor: an underfunded output returns false.
    // The traced build of this step exceeds the reference-script publication
    // target, so on the plain build the refusal is pinned to the one script
    // the transaction runs: the step's own thread spend.
    const refusal = await expectOnchainRefusal(
      async () =>
        await submitMinAdaUtxoStep03({
          ...f.common,
          threadOutRef: opened.nextThreadOutRef,
          outputItemCbors: f.prepared.outputItemCbors,
          coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
          referenceScriptUtxo: f.refs[2]!,
          unsafeSkipLocalViolationCheckForTest: true,
        }),
      /^Validator returned false$/u,
    );
    expect(refusal).toMatch(/failed script execution Spend\[0\]/u);
  }, 900_000);
});
