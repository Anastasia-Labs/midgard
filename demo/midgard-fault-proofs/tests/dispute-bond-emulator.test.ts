import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  DisputeBond,
  DisputeBondSpendRedeemer,
  protocolTreasuryAddress,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  paymentCredentialOf,
  type Script,
  scriptFromNative,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { applyBlueprintParamsExact } from "../src/runtime.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  expectOnchainRefusal,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const BOND_TITLE = "fraud_proofs/validation_trace/dispute_bond_v1.main.spend";
const bondLovelace = 10_000_000_000n;
const cap = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.max_dispute_move_fee_lovelace,
);
const headerHash = "aa".repeat(28);
const otherHeaderHash = "bb".repeat(28);
const forfeitDelayMs = 600_000;

type Scene = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly bondScript: Script;
  readonly bonds: readonly [UTxO, UTxO];
  readonly conviction: UTxO;
  readonly otherConviction: UTxO;
  readonly proverAddress: string;
  readonly forfeitAfter: number;
};

const tagOf = (utxo: UTxO) =>
  Data.to(new Constr(0, [utxo.txHash, BigInt(utxo.outputIndex)]));

/**
 * Two bonds on one block and two permanent fraud records, one convicting that
 * block and one convicting another, under a fraud-proof policy the wallet
 * controls. The validator is applied to that policy exactly as deployed.
 */
const setUp = async (): Promise<Scene> => {
  const wallet = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const prover = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const proverKey = paymentCredentialOf(prover.address).hash;
  const fraudProofPolicy = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(wallet.address).hash,
  });
  const fraudProofPolicyId = mintingPolicyToId(fraudProofPolicy);
  const bondScript: Script = {
    type: "PlutusV3",
    script: applyBlueprintParamsExact({
      blueprint: readBlueprint(realBlueprintPath),
      title: BOND_TITLE,
      params: [fraudProofPolicyId],
    }),
  };
  const bondAddress = validatorToAddress(network, bondScript);
  const forfeitAfter = emulator.now() + forfeitDelayMs;
  const bondDatum = Data.to(
    {
      challenged_header_hash: headerHash,
      fraud_prover: proverKey,
      forfeit_after: BigInt(forfeitAfter),
    },
    DisputeBond,
  );
  const record = Data.to(new Constr(0, [proverKey]));
  const unit = (header: string) => `${fraudProofPolicyId}00000006${header}`;
  const setup = await lucid
    .newTx()
    .mintAssets({ [unit(headerHash)]: 1n, [unit(otherHeaderHash)]: 1n })
    .attach.MintingPolicy(fraudProofPolicy)
    .pay.ToAddressWithData(
      wallet.address,
      { kind: "inline", value: record },
      { lovelace: 2_000_000n, [unit(headerHash)]: 1n },
    )
    .pay.ToAddressWithData(
      wallet.address,
      { kind: "inline", value: record },
      { lovelace: 2_000_000n, [unit(otherHeaderHash)]: 1n },
    )
    .pay.ToAddressWithData(
      bondAddress,
      { kind: "inline", value: bondDatum },
      { lovelace: bondLovelace },
    )
    .pay.ToAddressWithData(
      bondAddress,
      { kind: "inline", value: bondDatum },
      { lovelace: bondLovelace },
    )
    .complete();
  const setupHash = await (await setup.sign.withWallet().complete()).submit();
  await lucid.awaitTx(setupHash);
  const created = await lucid.utxosByOutRef(
    [0, 1, 2, 3].map((outputIndex) => ({ txHash: setupHash, outputIndex })),
  );
  const at = (outputIndex: number) =>
    created.find((utxo) => utxo.outputIndex === outputIndex)!;
  return {
    lucid,
    emulator,
    bondScript,
    bonds: [at(2), at(3)],
    conviction: at(0),
    otherConviction: at(1),
    proverAddress: credentialToAddress(network, {
      type: "Key",
      hash: proverKey,
    }),
    forfeitAfter,
  };
};

const refund = (outputIndex = 0n) =>
  Data.to(
    { Refund: { fraud_proof_ref_input_index: 0n, output_index: outputIndex } },
    DisputeBondSpendRedeemer,
  );

const forfeit = (outputIndex = 0n) =>
  Data.to({ Forfeit: { output_index: outputIndex } }, DisputeBondSpendRedeemer);

const treasuryAddress = () =>
  protocolTreasuryAddress(
    network,
    SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
  );

/** A spend of `bonds`, each paying `pay(bond)` tagged with its own out ref. */
const spendBonds = (
  scene: Scene,
  {
    bonds,
    redeemer,
    destination,
    pay,
    references = [],
    validFrom,
    bondPaysFee,
  }: {
    readonly bonds: readonly UTxO[];
    readonly redeemer: (index: bigint) => string;
    readonly destination: string;
    readonly pay: (bond: UTxO) => bigint;
    readonly references?: readonly UTxO[];
    readonly validFrom?: number;
    readonly bondPaysFee: boolean;
  },
) => {
  let tx = scene.lucid.newTx();
  if (references.length > 0) tx = tx.readFrom([...references]);
  bonds.forEach((bond, index) => {
    tx = tx
      .collectFrom([bond], redeemer(BigInt(index)))
      .pay.ToAddressWithData(
        destination,
        { kind: "inline", value: tagOf(bond) },
        { lovelace: pay(bond) },
      );
  });
  if (validFrom !== undefined) tx = tx.validFrom(validFrom);
  tx = tx.attach.SpendingValidator(scene.bondScript);
  if (!bondPaysFee) return tx.complete({ localUPLCEval: true });
  // A bond-paid fee is exactly the cap: the outputs leave exactly `cap`
  // unspent, so there is no change output, and the wallet only supplies
  // collateral.
  return tx
    .setMinFee(cap)
    .complete({ coinSelection: false, localUPLCEval: true });
};

const submit = async (scene: Scene, build: Promise<TxSignBuilder>) => {
  const signed = await (await build).sign.withWallet().complete();
  await scene.lucid.awaitTx(await signed.submit());
};

const pastForfeitAfter = (scene: Scene) => {
  scene.emulator.awaitSlot(forfeitDelayMs / 1000 + 20);
  return scene.emulator.now();
};

describe("dispute_bond_v1", () => {
  it("refunds a bond to the prover on conviction, the lone bond paying its fee", async () => {
    const scene = await setUp();
    const [bond] = scene.bonds;
    await submit(
      scene,
      spendBonds(scene, {
        bonds: [bond],
        redeemer: refund,
        destination: scene.proverAddress,
        pay: (spent) => BigInt(spent.assets.lovelace) - cap,
        references: [scene.conviction],
        bondPaysFee: true,
      }),
    );
    const paid = await scene.lucid.utxosAt(scene.proverAddress);
    expect(paid).toHaveLength(1);
    expect(paid[0]!.assets.lovelace).toBe(bondLovelace - cap);
    expect(paid[0]!.datum).toBe(tagOf(bond));
  });

  it("refuses a refund without the conviction of the challenged block, or paying another address", async () => {
    const scene = await setUp();
    const [bond] = scene.bonds;
    const honest = {
      bonds: [bond],
      redeemer: refund,
      destination: scene.proverAddress,
      pay: () => bondLovelace,
      references: [scene.conviction],
      bondPaysFee: false,
    } as const;
    await expectOnchainRefusal(() =>
      spendBonds(scene, { ...honest, references: [scene.otherConviction] }),
    );
    await expectOnchainRefusal(() =>
      spendBonds(scene, { ...honest, destination: treasuryAddress() }),
    );
    await submit(scene, spendBonds(scene, honest));
  });

  it("refuses two bonds that each pay the fee, and accepts the batch when a wallet pays it", async () => {
    const scene = await setUp();
    const batch = {
      bonds: scene.bonds,
      redeemer: refund,
      destination: scene.proverAddress,
      references: [scene.conviction],
    } as const;
    await expectOnchainRefusal(() =>
      spendBonds(scene, {
        ...batch,
        pay: (bond) => BigInt(bond.assets.lovelace) - cap / 2n,
        bondPaysFee: true,
      }),
    );
    await submit(
      scene,
      spendBonds(scene, {
        ...batch,
        pay: (bond) => BigInt(bond.assets.lovelace),
        bondPaysFee: false,
      }),
    );
    expect(await scene.lucid.utxosAt(scene.proverAddress)).toHaveLength(2);
  });

  it("refuses a forfeit before forfeit_after, or to any address but the treasury", async () => {
    const scene = await setUp();
    const [bond] = scene.bonds;
    await expectOnchainRefusal(() =>
      spendBonds(scene, {
        bonds: [bond],
        redeemer: forfeit,
        destination: treasuryAddress(),
        pay: (spent) => BigInt(spent.assets.lovelace) - cap,
        validFrom: scene.forfeitAfter,
        bondPaysFee: true,
      }),
    );
    const now = pastForfeitAfter(scene);
    await expectOnchainRefusal(() =>
      spendBonds(scene, {
        bonds: [bond],
        redeemer: forfeit,
        destination: scene.proverAddress,
        pay: (spent) => BigInt(spent.assets.lovelace) - cap,
        validFrom: now,
        bondPaysFee: true,
      }),
    );
    await submit(
      scene,
      spendBonds(scene, {
        bonds: [bond],
        redeemer: forfeit,
        destination: treasuryAddress(),
        pay: (spent) => BigInt(spent.assets.lovelace) - cap,
        validFrom: now,
        bondPaysFee: true,
      }),
    );
  });

  it("forfeits a bond to the treasury after forfeit_after, even on a convicted block", async () => {
    const scene = await setUp();
    const [bond] = scene.bonds;
    const now = pastForfeitAfter(scene);
    await submit(
      scene,
      spendBonds(scene, {
        bonds: [bond],
        redeemer: forfeit,
        destination: treasuryAddress(),
        pay: (spent) => BigInt(spent.assets.lovelace) - cap,
        references: [scene.conviction],
        validFrom: now,
        bondPaysFee: true,
      }),
    );
    const paid = await scene.lucid.utxosAt(treasuryAddress());
    expect(paid).toHaveLength(1);
    expect(paid[0]!.assets.lovelace).toBe(bondLovelace - cap);
    expect(paid[0]!.datum).toBe(tagOf(bond));
  });
});
