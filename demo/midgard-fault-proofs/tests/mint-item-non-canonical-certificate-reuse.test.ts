/**
 * Step 02 walks a Certified-tier mint field over several turns. Each turn is
 * submitted with the arguments the workflow submission passes, which hand in
 * no certificate: the first turn certifies the opening, every later turn
 * reuses that certificate, and the opening keeps a single certificate until
 * the walk reaches its terminal.
 */
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitMintItemNonCanonicalStep01Accepted,
  submitMintItemNonCanonicalStep02,
} from "../src/mint-item-non-canonical/index.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { fieldPreimageCertificateCount } from "./support/captured-step-turns.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  committedInclusions,
  evidenceOf,
  registeredContracts,
  witnessSetCborOf,
} from "./support/mint-item-non-canonical-lifecycle.js";
import {
  lateMalformedMintItems,
  mintField,
} from "./support/mint-item-vectors.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";

const MAXIMUM_TURNS = 40;

describe("mintItemNonCanonical step 02 over a Certified mint field", () => {
  it("reuses the field certificate already on chain on every turn after the first", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realMintItemNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, references, common } = registered;
    const items = lateMalformedMintItems();
    const tx = makeNativeTx({
      spendInputCbors: [],
      fee: 0n,
      mintPreimageCbor: mintField(...items),
    });
    const committed = await committedInclusions([tx]);
    const inclusion = committed.inclusions[0]!;
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: registered.catalogue,
      fixture: {
        transactionsRoot: committed.transactionsRoot,
        l2TransactionCount: 1n,
      },
    });
    const certificateReference = await registered.publishReferences();
    const evidence = evidenceOf(
      acceptedVerdictSubject(inclusion.nativeTxId),
      tx,
      items.length - 1,
    );
    expect(evidence.canonical).toBe(false);
    expect(evidence.carriage).toBe("Certified");
    const init = await registered.init(setup.fraudulentBlockOutRef);
    const bound = await submitMintItemNonCanonicalStep01Accepted({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network: "Custom",
      contracts,
      signer: harness.proverSigner,
      finding: evidence,
      threadUtxo: await registered.threadUtxoOf(
        init.result.txHash,
        init.result.firstStepOutputIndex,
      ),
      threadToken: {
        unit: init.result.computationThreadUnit,
        fraudulentHeaderHash: init.result.fraudulentHeaderHash,
      },
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: inclusion,
      referenceScriptUtxo: references[0]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const certificates = () =>
      fieldPreimageCertificateCount({
        lucid: harness.proverLucid,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      });

    const ownOutputs = createOwnOutputJournal();
    let threadOutRef = bound.nextThreadOutRef;
    const counts: bigint[] = [];
    for (let turn = 0; turn < MAXIMUM_TURNS; turn += 1) {
      const advanced = await submitMintItemNonCanonicalStep02({
        ...common(threadOutRef, 1),
        ownOutputs,
        evidence,
        nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
        witnessSetCompactCbor: witnessSetCborOf(tx),
        publishCarriage: evidence.carriage === "RawUtxo",
        certificateReferenceScriptUtxo: certificateReference,
      });
      expect(advanced.carriageTier).toBe("Certified");
      counts.push(await certificates());
      threadOutRef = advanced.nextThreadOutRef;
      if (advanced.terminal) break;
    }
    expect(counts.length).toBeGreaterThan(1);
    expect(counts.length).toBeLessThan(MAXIMUM_TURNS);
    expect(counts).toEqual(counts.map(() => 1n));
  }, 1_800_000);
});
