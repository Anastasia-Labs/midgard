/** The prover funding permit walking a thread move under its authority, as
 * the runtime bound walks it. Over the reveal, timeout and thread close a
 * funded watcher built and landed, it admits each move's thread (and a
 * close's case) as released custody and each move's own outputs. It admits
 * a timeout and a thread close our wallet pays, above the cap, whose thread
 * keeps all of its lovelace (less the receipt, for the close), beside the
 * wallet's change. It refuses: a fee above the deployment's per-move cap on
 * a move its thread pays, a wallet-paid move whose thread pays the fee too,
 * a move that also
 * carries a fraud slash or a VerifySource authority, custody paid beyond a
 * move's own outputs, a move that omits its thread or a close that omits
 * its case, any other contract input, a protocol top-up, a successor other
 * than its thread less the fee, a close that changes the case's value or
 * pays another receipt or leftover than case-v1 fixes, a move that pays
 * fewer outputs than its validators fix, a successor or leftover carrying a
 * reference script, and a mint other than a close's own burn and receipt.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import {
  grantValidationDisputeFundingAuthority,
  validationDisputeFunding,
} from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { VALIDATION_DISPUTE_MOVE_BINDING } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  bodyOutputs,
  disputeTerms,
  fundingPolicy,
  movesOf,
  openWalk,
  permitState,
  rebodied,
  refusalOf,
  regranted,
  spentInputs,
  walk,
} from "./support/validation-trace-dispute-funded-move-walk.js";
import {
  type CapturedMove,
  type FundedMoves,
  openDefendedFundedCase,
  playFundedMovesToClose,
} from "./support/validation-trace-dispute-funded-moves.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";
import { withCoin } from "./support/validation-trace-dispute-tampered-verify-source.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let journey: FundedMoves;
let reveal: CapturedMove;
let timeout: CapturedMove;
let close: CapturedMove;

const outRefOf = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const feeOf = (move: CapturedMove) => move.signed.toTransaction().body().fee();

/** The walk over `move` reaching its output at `index` changed by `edit`. */
const outputRefusal = async (
  move: CapturedMove,
  index: number,
  edit: (output: CML.TransactionOutput) => CML.TransactionOutput,
) => {
  const opened = await openWalk(journey, move);
  opened.admitOpener(spentInputs(move));
  return opened.output(edit(bodyOutputs(move.signed)[index]!));
};

/** `output` with no datum. */
const withoutDatum = (output: CML.TransactionOutput) =>
  CML.TransactionOutput.new(output.address(), output.amount(), null, null);

/** `output` also carrying a reference script. */
const withReferenceScript = (output: CML.TransactionOutput) =>
  CML.TransactionOutput.new(
    output.address(),
    output.amount(),
    output.datum() ?? null,
    CML.Script.new_native(
      CML.NativeScript.new_script_all(CML.NativeScriptList.new()),
    ),
  );

const RECEIPT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);

/** `move` paid by our wallet: one of its plain-Ada outputs spent beside the
 * move's inputs, a fee above the cap, the move's outputs as `edit` makes
 * them and the wallet's change at the funding key after them. */
const walletPaid = async (
  move: CapturedMove,
  edit: (outputs: CML.TransactionOutput[]) => CML.TransactionOutput[],
) => {
  const { proverLucid, proverSigner } = journey.scene.h;
  const funding = (await proverLucid.wallet().getUtxos()).find(
    ({ assets }) => Object.keys(assets).length === 1,
  )!;
  return await regranted(journey, move, (body) => {
    const inputs = body.inputs();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(funding.txHash),
        BigInt(funding.outputIndex),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    for (const output of [
      ...edit(bodyOutputs(move.signed)),
      CML.TransactionOutput.new(
        CML.Address.from_bech32(
          credentialToAddress(network, {
            type: "Key",
            hash: proverSigner.paymentKeyHash,
          }),
        ),
        CML.Value.from_coin(2_000_000n),
        null,
        null,
      ),
    ])
      outputs.add(output);
    return rebodied(body, {
      inputs,
      outputs,
      fee: VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE + 1n,
    });
  });
};

describe("validation-trace dispute thread move walked under its authority", () => {
  beforeAll(async () => {
    journey = await openDefendedFundedCase();
    await playFundedMovesToClose(journey);
    reveal = movesOf(journey, "reveal")[0]!;
    timeout = movesOf(journey, "timeout")[0]!;
    close = movesOf(journey, "close_thread")[0]!;
  }, TIMEOUT);

  it("admits each move's spent thread and case as released custody and its own outputs, the leftover outside custody", async () => {
    for (const move of [reveal, timeout]) {
      const { lineages, custody } = await walk(journey, move);
      expect(lineages).toStrictEqual([
        {
          sourceActionKind: move.authority.kind,
          sourceOutputIndex: move.thread.outputIndex,
          outRef: outRefOf(move.thread),
          resolvedOutputCborHex: exactResolvedOutputCbor(move.thread),
        },
      ]);
      expect(custody).toStrictEqual([
        {
          custody: (move.thread.assets.lovelace ?? 0n) - feeOf(move),
          allocation: 0n,
        },
      ]);
    }
    const { lineages, custody } = await walk(journey, close);
    expect(
      lineages.map((lineage) => (lineage as { outRef: string }).outRef),
    ).toStrictEqual([outRefOf(close.thread), outRefOf(close.caseInput!)]);
    const outputs = bodyOutputs(close.signed);
    expect(custody).toStrictEqual([
      { custody: outputs[0]!.amount().coin(), allocation: 0n },
      { custody: outputs[1]!.amount().coin(), allocation: 0n },
      { custody: 0n, allocation: 0n },
    ]);
  });

  it("admits a fee its thread pays at the deployment's per-move cap and refuses one above it", async () => {
    const at = (cap: bigint) =>
      fundingPolicy(
        journey,
        disputeTerms(journey, { moveFeeCapLovelace: cap.toString() }),
      );
    expect(
      await refusalOf(() =>
        walk(journey, reveal, { policy: at(feeOf(reveal)) }),
      ),
    ).toBeNull();
    expect(
      await refusalOf(() =>
        walk(journey, reveal, { policy: at(feeOf(reveal) - 1n) }),
      ),
    ).toBe(`${VALIDATION_DISPUTE_MOVE_BINDING} (fee)`);
  });

  it("admits a timeout and a thread close our wallet pays above the cap, the timeout's successor keeping all of its thread's lovelace and the close's leftover all of it less the receipt", async () => {
    const kept = timeout.thread.assets.lovelace ?? 0n;
    const timedOut = await walk(journey, timeout, {
      signed: await walletPaid(timeout, ([successor]) => [
        withCoin(successor!, kept),
      ]),
    });
    expect(timedOut.custody).toStrictEqual([
      { custody: kept, allocation: 0n },
      null,
    ]);
    const closed = await walk(journey, close, {
      signed: await walletPaid(close, ([caseOutput, receipt, leftover]) => [
        caseOutput!,
        receipt!,
        withCoin(
          leftover!,
          (close.thread.assets.lovelace ?? 0n) - RECEIPT_LOVELACE,
        ),
      ]),
    });
    const outputs = bodyOutputs(close.signed);
    expect(closed.custody).toStrictEqual([
      { custody: outputs[0]!.amount().coin(), allocation: 0n },
      { custody: outputs[1]!.amount().coin(), allocation: 0n },
      { custody: 0n, allocation: 0n },
      null,
    ]);
  });

  it.each([
    {
      name: "a timeout our wallet pays whose successor keeps its thread less the fee",
      check: async () =>
        await walk(journey, timeout, {
          signed: await walletPaid(timeout, (outputs) => outputs),
        }),
      refusal:
        "validation dispute move successor differs from its thread less the fee",
    },
    {
      name: "a thread close our wallet pays whose leftover keeps its thread less receipt and fee",
      check: async () =>
        await walk(journey, close, {
          signed: await walletPaid(close, (outputs) => outputs),
        }),
      refusal:
        "validation dispute thread close leftover differs from its thread less receipt and fee",
    },
    {
      name: "a move that also carries a fraud slash",
      check: () => walk(journey, reveal, { slash: true }),
      refusal:
        "funding transaction carries both a fraud slash and a validation dispute move authority",
    },
    {
      name: "a move that carries no move authority",
      check: async () =>
        validationDisputeFunding({
          signed: await journey.scene.h.proverLucid
            .fromTx(reveal.signed.toTransaction().to_cbor_hex())
            .complete(),
          slash: false,
          policy: fundingPolicy(journey),
          state: permitState(journey, reveal),
          action: reveal.action,
        }),
      refusal: "validation dispute move carries no move authority",
    },
    {
      name: "a move that also carries a VerifySource authority",
      check: async () => {
        const signed = await regranted(journey, reveal, () => {});
        const { address } = reveal.thread;
        grantValidationDisputeFundingAuthority({
          signed,
          deploymentFingerprint: reveal.authority.deploymentFingerprint,
          headerHash: reveal.authority.headerHash,
          opener: reveal.thread,
          bond: { address, unit: "", lovelace: 0n, datumCborHex: "" },
          caseOutput: { address, unit: "", floorLovelace: 0n, lovelace: 0n },
        });
        return await walk(journey, reveal, { signed });
      },
      refusal:
        "funding transaction carries both a VerifySource and a thread move authority",
    },
    {
      name: "a move that pays custody beyond its thread's successors",
      check: async () => (await openWalk(journey, reveal)).refuseOtherCustody(),
      refusal:
        "validation dispute move pays custody beyond its thread's successors",
    },
    {
      name: "a move that omits its thread",
      check: async () =>
        (await openWalk(journey, reveal)).admitOpener(new Map()),
      refusal: "validation dispute move omits its thread",
    },
    {
      name: "a thread close that omits its case",
      check: async () =>
        (await openWalk(journey, close)).admitOpener(
          new Map([[outRefOf(close.thread), close.thread]]),
        ),
      refusal: "validation dispute thread close omits its case",
    },
    {
      name: "a reveal that consumes our case besides its thread",
      check: async () => {
        const opened = await openWalk(journey, reveal);
        opened.admitOpener(spentInputs(reveal));
        return opened.admitContractInput(
          outRefOf(close.caseInput!),
          close.caseInput!,
        );
      },
      refusal:
        "validation dispute move consumes a contract input besides its thread and case",
    },
    {
      name: "a move that tops up protocol state",
      check: async () =>
        (await openWalk(journey, reveal)).refuseProtocolTopup(1n),
      refusal: "validation dispute move tops up protocol state: topup=1",
    },
    {
      name: "a reveal that pays no successor",
      check: async () => {
        const opened = await openWalk(journey, reveal);
        opened.admitOpener(spentInputs(reveal));
        opened.assertComplete();
      },
      refusal: "validation dispute reveal must pay exactly successor: paid=",
    },
    {
      name: "a thread close that pays no receipt",
      check: async () => {
        const opened = await openWalk(journey, close);
        opened.admitOpener(spentInputs(close));
        const outputs = bodyOutputs(close.signed);
        opened.output(outputs[0]!);
        opened.output(outputs[2]!);
        opened.assertComplete();
      },
      refusal:
        "validation dispute close_thread must pay exactly case, leftover, receipt: paid=case, leftover",
    },
    {
      name: "a reveal that mints a close's burn and receipt",
      check: async () =>
        await walk(journey, reveal, {
          signed: await regranted(journey, reveal, (body) =>
            body.set_mint(close.signed.toTransaction().body().mint()!),
          ),
        }),
      refusal:
        "validation dispute reveal mints other than its thread's own tokens",
    },
    {
      name: "a thread close that mints nothing",
      check: async () =>
        await walk(journey, close, {
          signed: await regranted(journey, close, (body) =>
            body.set_mint(CML.Mint.new()),
          ),
        }),
      refusal:
        "validation dispute close_thread mints other than its thread's own tokens",
    },
    {
      name: "a reveal's successor one lovelace above its thread less the fee",
      check: () =>
        outputRefusal(reveal, 0, (output) =>
          withCoin(output, output.amount().coin() + 1n),
        ),
      refusal:
        "validation dispute move successor differs from its thread less the fee",
    },
    {
      name: "a reveal's successor without its inline datum",
      check: () => outputRefusal(reveal, 0, withoutDatum),
      refusal:
        "validation dispute move successor differs from its thread less the fee",
    },
    {
      name: "a reveal's successor carrying a reference script",
      check: () => outputRefusal(reveal, 0, withReferenceScript),
      refusal:
        "validation dispute move successor differs from its thread less the fee",
    },
    {
      name: "a timeout's successor one lovelace below its thread less the fee",
      check: () =>
        outputRefusal(timeout, 0, (output) =>
          withCoin(output, output.amount().coin() - 1n),
        ),
      refusal:
        "validation dispute move successor differs from its thread less the fee",
    },
    {
      name: "a thread close's case one lovelace richer",
      check: () =>
        outputRefusal(close, 0, (output) =>
          withCoin(output, output.amount().coin() + 1n),
        ),
      refusal: "validation dispute thread close changes the case's value",
    },
    {
      name: "a thread close's case without its datum",
      check: () => outputRefusal(close, 0, withoutDatum),
      refusal: "validation dispute thread close changes the case's value",
    },
    {
      name: "a thread close's receipt one lovelace richer",
      check: () =>
        outputRefusal(close, 1, (output) =>
          withCoin(output, output.amount().coin() + 1n),
        ),
      refusal:
        "validation dispute thread close receipt differs from its exact receipt",
    },
    {
      name: "a thread close's receipt without its datum",
      check: () => outputRefusal(close, 1, withoutDatum),
      refusal:
        "validation dispute thread close receipt differs from its exact receipt",
    },
    {
      name: "a thread close's leftover one lovelace short",
      check: () =>
        outputRefusal(close, 2, (output) =>
          withCoin(output, output.amount().coin() - 1n),
        ),
      refusal:
        "validation dispute thread close leftover differs from its thread less receipt and fee",
    },
    {
      name: "a thread close's leftover carrying a reference script",
      check: () => outputRefusal(close, 2, withReferenceScript),
      refusal:
        "validation dispute thread close leftover differs from its thread less receipt and fee",
    },
  ])("refuses $name", async ({ check, refusal }) => {
    expect(await refusalOf(check)).toBe(refusal);
  });
});
