import "./utils.js";

import { writeFile } from "node:fs/promises";

import { decodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  deriveMidgardTxFieldPreimages,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core/consensus-validation";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect, it, vi } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import { Columns as LedgerColumns } from "../src/database/utils/ledger.js";
import { forcedOrderEntry } from "../src/forced-orders/entry.js";
import { MidgardMpf, processMpfs } from "../src/mpf/index.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import { resolveForcedTransactionsRoot } from "../src/workers/commit-block-header/event-roots.js";
import {
  hashBlockHeaderLocal,
  updateLatestBlocksDatumAndGetTheNewHeaderLocal,
} from "../src/workers/commit-block-header/state-queue.js";
import {
  commitDaPayloadForSizing,
  daProgramMaterialFromSidecars,
  forcedProgramMaterialSidecars,
} from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import { makeEventCommitments } from "../src/workers/commit-block-header/transition-commitments.js";
import {
  delayProgram,
  openNativeFrameFixture,
  programScript,
  runNativeFrameDatabase,
} from "./helpers/commit-additive-native-fixture.js";
import { openPublishedLifecycle } from "./helpers/published-lifecycle.js";

/** Real current validators admit the signed L1 order. The L2 supplier uses
 * real signatures/validation/native roots, but its ConfirmedState parent is a
 * model: this proves the material boundary, not an authenticated L2 landing. */
it("admits a native-only order referencing a verified CEK supplier and emits no forced program material", async () => {
  const l1 = await openPublishedLifecycle();
  const h = await openNativeFrameFixture({ timeMs: l1.fixture.emulator.now() });
  try {
    const program = delayProgram(32);
    const supplierOutputs = [
      h.output(2_000_000n, 0, programScript(program)),
      h.output(49_998_000_000n),
    ];
    const supplier = h.signed(h.inputRef, [], supplierOutputs);
    await h.validate(supplier, h.initial, program.material);
    const supplied = await h.build(
      supplier,
      h.initial,
      h.root,
      program.material,
    );
    expect(supplied.processed.processedMempoolTxs).toHaveLength(1);
    expect(supplied.builds).toBe(1);
    await h.exact(supplied.processed, h.root);
    await h.native.promote(supplied.handle);
    const base = supplierOutputs.map((output, outputIndex) => ({
      [LedgerColumns.OUTREF]: encodeMidgardSpendInputItem({
        txId: supplier.txId,
        outputIndex,
      }),
      [LedgerColumns.OUTPUT]: output,
    }));
    const consumer = h.signed(
      base[1]![LedgerColumns.OUTREF],
      [base[0]![LedgerColumns.OUTREF]],
      [h.output(49_998_000_000n)],
    );
    const forced = materializeMidgardForcedTxFromCanonical(consumer.tx);
    expect(screenMidgardForcedTxAdmission(forced)).toBeNull();
    const cbor = encodeMidgardForcedTxCanonical(forced);
    const wallet = l1.fixture.depositorLucid;
    const funding = await wallet.wallet().getUtxos();
    const nonceInput = funding.find((u) => u.datum == null);
    if (nonceInput === undefined)
      throw new Error("published wallet has no nonce");
    const orderBuild = await Effect.runPromise(
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(
        wallet,
        l1.fixture.contracts,
        {
          submittedTxCbor: cbor.toString("hex"),
          nonceInput,
          refundAddress: await Effect.runPromise(
            SDK.addressDataFromBech32(await wallet.wallet().address()),
          ),
          // The current deployment catalogue does not publish txOrderMint.
          // The SDK attaches this actual validator when no reference is supplied.
        },
      ),
    );
    const signed = await orderBuild.tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    expect(await wallet.awaitTx(txHash)).toBe(true);
    const order = (
      await Effect.runPromise(
        SDK.utxosToTxOrderUTxOs(
          await wallet.utxosAt(
            l1.fixture.contracts.txOrder.spendingScriptAddress,
          ),
          l1.fixture.contracts.txOrder.policyId,
        ),
      )
    ).find((o) => o.utxo.txHash === txHash);
    expect(order).toBeDefined();
    if (order === undefined)
      throw new Error("accepted order not authenticated by SDK");
    const row = await Effect.runPromise(
      forcedOrderEntry({
        order,
        fieldPreimages: deriveMidgardTxFieldPreimages(cbor, "forced").map(
          (f) => f.preimageCbor,
        ),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    );
    const clock = Number(order.datum.inclusion_time) + 1_000;
    await l1.deployment.chain.awaitLedgerTime(clock);
    await runNativeFrameDatabase(
      ForcedTransactionsDB.insertEntries([row]).pipe(
        Effect.provideService(FollowerWriteFixture, true),
      ),
    );
    const handle = await h.native.fork(supplied.processed.utxoRoot);
    let builds = 0;
    const client = {
      fork: h.native.fork.bind(h.native),
      discard: h.native.discard.bind(h.native),
      applyEvents: (...args: Parameters<typeof h.native.applyEvents>) => {
        builds += 1;
        return h.native.applyEvents(...args);
      },
    };
    const txMpf = await runNativeFrameDatabase(
      MidgardMpf.createScratch("forced-reference-80"),
    );
    const processed = await runNativeFrameDatabase(
      processMpfs(txMpf, [], {
        currentBlockStartTime: new Date(h.time - 1_000),
        fixedBlockEndTime: new Date(clock),
        fixedBlockTime: new Date(clock),
        initialLedgerEntries: base,
        selectedBaseUtxoRoot: supplied.processed.utxoRoot,
        payloadRootCheck: "off",
        nativeMpf: { client, handle, ownerBinarySha256: "00".repeat(32) },
        forcedValidation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          l1SlotConfig: h.slotConfig,
          slotForUnixTime: (ms) =>
            BigInt(
              Math.floor(
                (ms - h.slotConfig.zeroTime) / h.slotConfig.slotLength,
              ) + h.slotConfig.zeroSlot,
            ),
        },
      }).pipe(Effect.provideService(FollowerWriteFixture, true)),
    );
    expect(processed.includedForcedTransactionEntries).toHaveLength(1);
    expect(processed.processedMempoolTxs).toHaveLength(0);
    expect(builds).toBe(1);
    const emitted = processed.includedForcedTransactionEntries[0]!;
    expect(
      decodeMidgardCekProgramMaterialSidecar(
        emitted[
          ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
        ]!,
      ),
    ).toEqual([]);
    const leaf = Data.from(
      emitted[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE].toString(
        "hex",
      ),
      SDK.ForcedInclusionTxV1,
    );
    expect(leaf.verdict).toBe("ForcedTxValid");
    const forcedRoot = await Effect.runPromise(
      resolveForcedTransactionsRoot(processed.includedForcedTransactionEntries),
    );
    const commitments = await Effect.runPromise(
      makeEventCommitments(
        {
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: Option.getOrThrow(forcedRoot),
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transitionTraceRoot: processed.transitionTraceRoot,
          eventToStepRoot: processed.eventToStepRoot,
        },
        {
          withdrawalCount: 0n,
          forcedTransactionCount: 1n,
          l2TransactionCount: 0n,
          depositCount: 0n,
          transitionStepCount: 1n,
        },
        {
          validationTracesRoot: processed.validationTracesRoot,
          validationTraceCount: 1n,
        },
      ),
    );
    const { header } = await Effect.runPromise(
      updateLatestBlocksDatumAndGetTheNewHeaderLocal(
        h.lucid.api,
        h.parent(supplied.processed.utxoRoot).datum,
        processed.utxoRoot,
        SDK.EMPTY_MERKLE_TREE_ROOT,
        SDK.EMPTY_MERKLE_TREE_ROOT,
        SDK.EMPTY_MERKLE_TREE_ROOT,
        commitments,
        BigInt(clock),
        {
          blockSlot: BigInt(
            Math.floor(
              (clock - h.slotConfig.zeroTime) / h.slotConfig.slotLength,
            ) + h.slotConfig.zeroSlot,
          ),
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
        },
      ),
    );
    const headerHash = await Effect.runPromise(hashBlockHeaderLocal(header));
    const payload = await Effect.runPromise(
      commitDaPayloadForSizing({
        ...processed,
        header,
        headerHash,
        cekProgramMaterial: daProgramMaterialFromSidecars(
          forcedProgramMaterialSidecars(
            processed.includedForcedTransactionEntries,
          ),
        ),
      }),
    );
    expect(payload.block_body.cek_program_material).toEqual([]);
    await writeFile(
      "/tmp/c2b-forced-reference-actual80-material-boundary.json",
      JSON.stringify(
        {
          base: "80fb45a3633c1ea2410ec3b916deb8afc20c78fd",
          scope:
            "real L1 forced order; native verified supplier with model ConfirmedState parent; no L2 landing claim",
          signedOrderCbor: signed.toCBOR(),
          txHash,
          preState: base.map((r) => ({
            key: r[LedgerColumns.OUTREF].toString("hex"),
            value: r[LedgerColumns.OUTPUT].toString("hex"),
          })),
          supplierCanonicalCbor: supplier.txCbor.toString("hex"),
          forcedCanonicalCbor: cbor.toString("hex"),
          payload,
        },
        (_, v) => (typeof v === "bigint" ? v.toString() : v),
        2,
      ),
    );
    await h.native.discard(handle);
    await runNativeFrameDatabase(txMpf.close());
  } finally {
    await h.close();
    vi.useRealTimers();
  }
}, 900_000);
