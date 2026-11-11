/** The non-existent-input route with every step built under the watcher's
 * restricted prover wallet. That wallet offers coin selection its reserved
 * plain funding and keeps its reserved collateral apart, so a step that hands
 * Lucid a needless preset loses that collateral and pays it from funding,
 * which the reservation refuses. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import { submitInputExistenceStep } from "../src/input-existence/submit.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { runInputExistenceRoute } from "./support/emulator/input-existence-route.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  fundReservedCollateral,
  restrictedProverFunding,
} from "./support/restricted-prover-funding.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

describe("non-existent-input steps under the watcher's restricted wallet", () => {
  it("takes every step's collateral from the reserved collateral and convicts", async () => {
    const h = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realNonExistentInput: true,
        alwaysFraudProofCatalogue: true,
      },
      registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
    });
    const nativeTx = makeNativeTx({
      spendInputCbors: [
        encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0x66),
          outputIndex: 0,
        }),
      ],
      fee: 0n,
    });
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(h.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: { kind: "normal", nativeTx },
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(block),
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "emulator/input-existence",
        grade: "security",
      },
    });
    const setup = await submitSetupTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      nonceUtxo: h.nonceUtxo,
      catalogue: h.catalogue,
      header: evidence.header,
    });
    const prepared = await prepareInputExistence({
      block: evidence,
      eventKey: { L2TransactionEventKey: { tx_id: block.nativeTxId } },
      field: 0,
      inputIndex: 0n,
      predecessor: await checkLedgerAgainstRoot({
        entries: [],
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        label: "restricted-wallet prior-block ledger",
      }),
      owner: h.proverSigner.paymentKeyHash,
    });
    const collateral = await fundReservedCollateral({
      emulator: h.emulator,
      funderLucid: h.funderLucid,
      signer: h.proverSigner,
    });
    let checked = 0;
    await runInputExistenceRoute({
      h,
      field: 0,
      prepared,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      // A fresh reservation per step, as the watcher begins each action.
      submitStep: async (args) => {
        const funding = await restrictedProverFunding({
          emulator: h.emulator,
          signer: args.signer,
          collateral,
        });
        const result = await submitInputExistenceStep({
          ...args,
          signer: funding.signer,
          preSubmitBoundary: funding.preSubmitBoundary,
        });
        checked += funding.checked();
        return result;
      },
      outcome: { kind: "conviction" },
    });
    expect(checked).toBe(4);
  }, 600_000);
});
