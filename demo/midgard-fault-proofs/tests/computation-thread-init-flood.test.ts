/** Many computation threads opened against an honest block by a fresh key. */
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitLinearFaultCancel } from "../src/linear-fault-cancel.js";
import { parseOutRef } from "../src/runtime.js";
import { submitInit } from "../src/submit-init.js";
import { stageDoubleSpendStep04 } from "./support/submit-init-emulator-fixtures.stage-double-spend-step04.js";
import { network } from "./support/submit-init-emulator-shared.js";

const THREADS_PER_ROUND = 8;

describe("computation-thread Init flood against an honest block", () => {
  it("leaves block production running while the threads stay open, and costs the opener a fee per thread", async () => {
    const staged = await stageDoubleSpendStep04({
      honestBlock: true,
      seeds: { prover: generateSeedPhrase() },
    });
    const { proverLucid: lucid, proverSigner: signer, successors } = staged;
    const walletLovelace = async () => {
      signer.selectWallet(lucid);
      return (await lucid.wallet().getUtxos()).reduce(
        (total, utxo) => total + utxo.assets.lovelace,
        0n,
      );
    };
    const blockOutRef = () =>
      successors.at(-1)?.continuedAnchorOutRef ?? staged.fraudulentBlockOutRef;
    const threads: { outRef: string; lovelace: bigint; fee: bigint }[] = [];
    const flood = async () => {
      for (let index = 0; index < THREADS_PER_ROUND; index += 1) {
        const before = await walletLovelace();
        const init = await submitInit({
          lucid,
          blueprint: staged.realBlueprint,
          deploymentInfo: staged.deploymentInfo,
          network,
          signer,
          fraudulentBlockOutRef: blockOutRef(),
          witnessReferenceScripts: staged.witnessReferenceScripts,
          awaitConfirmation: true,
        });
        expect(init.fraudulentHeaderHash).toBe(staged.headerHash);
        const outRef = `${init.txHash}#${init.firstStepOutputIndex}`;
        const [thread] = await lucid.utxosByOutRef([parseOutRef(outRef)]);
        const lovelace = thread!.assets.lovelace;
        threads.push({
          outRef,
          lovelace,
          fee: before - (await walletLovelace()) - lovelace,
        });
      }
    };

    // Two rounds of Inits, each followed by the operator committing the next
    // block: the Inits only read the block, so the commit that re-creates its
    // state-queue output goes through untouched by every open thread.
    await flood();
    await staged.appendSuccessors(1, true);
    await flood();
    await staged.appendSuccessors(1, true);
    expect(successors).toHaveLength(2);
    expect(threads).toHaveLength(2 * THREADS_PER_ROUND);

    // The opener takes back each thread's lovelace by cancelling it, so the
    // flood costs it the Init and cancel fees alone.
    const before = await walletLovelace();
    let cancelFees = 0n;
    for (const { outRef, lovelace } of threads) {
      const walletBefore = await walletLovelace();
      const cancelled = await submitLinearFaultCancel({
        lucid,
        family: "double-spend",
        steps: staged.contracts.fraudProofContracts.doubleSpend.steps,
        computationThread: staged.contracts.computationThread,
        categoryId: staged.catalogue.categories.doubleSpend.categoryId,
        signer,
        threadOutRef: outRef,
        referenceScriptUtxo:
          staged.doubleSpendStepReferenceScripts["fraudProofDoubleSpend"]!.utxo,
        witnessReferenceScripts: staged.witnessReferenceScripts,
      });
      expect(cancelled.reclaimedLovelace).toBe(lovelace);
      cancelFees += lovelace - ((await walletLovelace()) - walletBefore);
    }
    const reclaimed = threads.reduce((sum, { lovelace }) => sum + lovelace, 0n);
    expect((await walletLovelace()) - before).toBe(reclaimed - cancelFees);

    const initFees = threads.map(({ fee }) => fee);
    const lockedPerThread = [
      ...new Set(threads.map(({ lovelace }) => lovelace)),
    ];
    console.info(
      `[computation-thread-init-flood] ${JSON.stringify({
        threads: threads.length,
        initFeeMin: String(initFees.reduce((a, b) => (a < b ? a : b))),
        initFeeMax: String(initFees.reduce((a, b) => (a > b ? a : b))),
        lockedPerThread: lockedPerThread.map(String),
        cancelFeeMean: String(cancelFees / BigInt(threads.length)),
      })}`,
    );
  }, 600_000);
});
