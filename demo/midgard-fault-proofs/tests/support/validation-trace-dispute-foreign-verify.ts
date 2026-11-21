/**
 * Another party verifying our pending claim first, under the prover funding
 * permit, on the Open scene's emulator: our funded watcher opens our claim
 * and signs our VerifySource, and its process stops before broadcasting it;
 * another party, holding a bond's worth and fees, then runs VerifySource
 * over our pending claim, so the case our Open opened is its.
 */
import {
  CML,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import { recordedOutput } from "./validation-trace-dispute-funded-moves.js";
import {
  type AdapterWrap,
  type FundedDispute,
  turnUntil,
} from "./validation-trace-dispute-funded-runner.js";
import { isVerifySource } from "./validation-trace-dispute-funded-verify-source.js";
import {
  BOND_LOVELACE,
  failingMaterial,
  honestPresentation,
  type Scene,
  verifySource,
} from "./validation-trace-dispute-open-scene.js";

const ADA = 1_000_000n;

export const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The out-refs `signedHex` spends. */
export const spentBy = (signedHex: string): readonly string[] => {
  const inputs = CML.Transaction.from_cbor_hex(signedHex).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/**
 * Runs the watcher until it signs our VerifySource and journals it, then
 * stops its process before broadcasting; the stopped turn never returns.
 */
const stopBeforeVerifySourceBroadcast = async (dispute: FundedDispute) => {
  let signedHex: string | undefined;
  let stopped!: () => void;
  const stop = new Promise<void>((resolve) => {
    stopped = resolve;
  });
  const wrap: AdapterWrap = (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      if (isVerifySource(context.action))
        signedHex = workflowPreflightTransaction(preflight)!
          .toTransaction()
          .to_cbor_hex();
      return preflight;
    },
    submit: async (input) => {
      if (!isVerifySource(input.action)) return await adapter.submit(input);
      stopped();
      return await new Promise<never>(() => undefined);
    },
  });
  const turns = turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, { wrap });
  await Promise.race([
    stop,
    turns.then(({ history }) => {
      throw new Error(`the process ended before submit: ${history.join("\n")}`);
    }),
  ]);
  if (signedHex === undefined) throw new Error("VerifySource was not signed");
  return signedHex;
};

/** A party other than us, holding a bond's worth and fees. */
export const anotherParty = async (scene: Scene) => {
  const { h } = scene;
  const account = generateEmulatorAccount({ lovelace: 0n });
  const funding = await h.funderLucid
    .newTx()
    .pay.ToAddress(account.address, { lovelace: BOND_LOVELACE + 500n * ADA })
    .complete();
  await h.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(h.emulator, "Custom", {
    slotConfig: h.proverLucid.config().slotConfig,
  });
  lucid.selectWallet.fromSeed(account.seedPhrase);
  return lucid;
};

/**
 * Our stopped VerifySource's signed bytes and the wallet inputs it holds
 * besides our claim, and the case and bond the other party's VerifySource
 * created over that claim.
 */
export const verifiedByAnotherParty = async (
  scene: Scene,
  dispute: FundedDispute,
) => {
  const { h, chain } = scene;
  const stoppedHex = await stopBeforeVerifySourceBroadcast(dispute);
  const spent = spentBy(stoppedHex);
  const wallet = new Set(
    (await h.emulator.getUtxos(h.proverSigner.address)).map(outRefOf),
  );
  // Our stopped VerifySource holds wallet inputs besides our claim.
  const held = spent.filter((outRef) => wallet.has(outRef));
  expect(held.length).toBeGreaterThan(0);
  const claims = spent.filter((outRef) => !wallet.has(outRef));
  expect(claims).toHaveLength(1);

  const material = await failingMaterial(scene);
  const lucid = await anotherParty(scene);
  const foreign = verifySource(scene, recordedOutput(scene, claims[0]!), {
    ...honestPresentation(material),
    lucid,
  });
  const foreignHash = await (
    await (await foreign.build()).sign.withWallet().complete()
  ).submit();
  await lucid.awaitTx(foreignHash);
  const [caseOutput, bond] = await h.emulator.getUtxosByOutRef([
    parseOwnOutRef(`${foreignHash}#0`),
    parseOwnOutRef(`${foreignHash}#1`),
  ]);
  expect(caseOutput!.address).toBe(chain.case.spendingScriptAddress);
  expect(bond!.address).toBe(chain.disputeBond.spendingScriptAddress);
  return {
    stoppedHex,
    held,
    material,
    caseOutput: caseOutput!,
    bond: bond!,
  } as const;
};
