/**
 * Offline owner-quorum primitives. The transaction under test is a real
 * `BeginWithdraw` the SDK builder produced on a Lucid emulator, with the
 * shared always-succeeds script in the pool's place (the pool validator's own
 * checks run in the node's emulator lifecycle suites). Owner witnesses are
 * made with only their own key; the assembled transaction is then submitted
 * to the emulator, which checks every signature and required signer.
 */
import { readFileSync } from "node:fs";

import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  availabilityResponseGeometry,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityParameters,
} from "../src/availability-challenge.js";
import type { AuthenticatedValidator } from "../src/common.js";
import { DaParamsDatum } from "../src/da-attestation.js";
import { daBondPoolUnit, decodeDaBondPoolDatum } from "../src/da-bond-pool.js";
import {
  assembleDaBondPoolTx,
  assertDaBondPoolWitnessQuorum,
  daBondPoolQuorumShortfallMessage,
  daBondPoolTxBodyHash,
  daBondPoolTxRequiredSigners,
  verifiedDaBondPoolWitnessKeyHashes,
  witnessDaBondPoolTx,
} from "../src/da-bond-pool-quorum.js";
import {
  buildBeginDaBondPoolWithdrawTxProgram,
  buildInitDaBondPoolTxProgram,
  DaBondPoolBuildError,
  type DaBondPoolBuildFailureReason,
} from "../src/da-bond-pool-transactions.js";
import * as Sdk from "../src/index.js";

const ADA = 1_000_000n;
const NETWORK: Network = "Custom";
const WITHDRAW_DELAY_MS = 60_000n;

const parameters = daAvailabilityParameters({
  responseGeometry: availabilityResponseGeometry(
    DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  ),
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

const alwaysSucceedsScript = (title: string): Script => {
  const validator = alwaysSucceedsBlueprint.validators.find(
    (entry) => entry.title === title,
  );
  if (validator === undefined) {
    throw new Error(`always-succeeds blueprint has no ${title}`);
  }
  return {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(validator.compiledCode),
  };
};

const standInPool = (): AuthenticatedValidator => {
  const script = alwaysSucceedsScript(
    alwaysSucceedsBlueprint.validators[0]!.title,
  );
  return {
    policyId: mintingPolicyToId(script),
    spendingScriptAddress: validatorToAddress(NETWORK, script),
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptCBOR: script.script,
    mintingScriptCBOR: script.script,
    spendingScript: script,
    mintingScript: script,
  };
};

type Key = { readonly bech32: string; readonly keyHash: string };

const keyOf = (privateKey: CML.PrivateKey): Key => {
  const key = {
    bech32: privateKey.to_bech32(),
    keyHash: privateKey.to_public().hash().to_hex(),
  };
  privateKey.free();
  return key;
};

type Scene = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly unit: string;
  readonly poolAddress: string;
  readonly feePayer: Key;
  readonly ownerA: Key;
  readonly ownerB: Key;
  readonly stranger: Key;
  /** Unsigned `BeginWithdraw`, required signers `[ownerA, ownerB]`. */
  readonly txCbor: string;
  readonly txHash: string;
};

const daParams = (
  owners: readonly Key[],
  updateThreshold: bigint,
): Pick<DaParamsDatum, "owners" | "update_threshold"> => ({
  owners: owners.map((owner) => owner.keyHash),
  update_threshold: updateThreshold,
});

const setupScene = async (): Promise<Scene> => {
  const account = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000n * ADA,
  });
  const emulator = new Emulator([account], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxCollateralInputs: 3,
  });
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromPrivateKey(account.privateKey);
  const feePayer: Key = {
    bech32: account.privateKey,
    keyHash: CML.PrivateKey.from_bech32(account.privateKey)
      .to_public()
      .hash()
      .to_hex(),
  };
  // One extended and one normal key, so both bech32 forms are exercised.
  const ownerA = keyOf(CML.PrivateKey.generate_ed25519extended());
  const ownerB = keyOf(CML.PrivateKey.generate_ed25519());
  const stranger = keyOf(CML.PrivateKey.generate_ed25519());
  const pool = standInPool();
  const unit = daBondPoolUnit(pool.policyId);

  const paramsDatum: DaParamsDatum = {
    committee: "",
    committee_signers_hash: "00".repeat(32),
    da_threshold: 1n,
    owners: [ownerA.keyHash, ownerB.keyHash],
    update_threshold: 2n,
  };
  const submit = async (signed: { submit: () => Promise<string> }) => {
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    emulator.awaitBlock(1);
    return hash;
  };
  const paramsHash = await submit(
    await (
      await lucid
        .newTx()
        .pay.ToContract(
          validatorToAddress(
            NETWORK,
            alwaysSucceedsScript("midgard.always_fail.else"),
          ),
          { kind: "inline", value: Data.to(paramsDatum, DaParamsDatum) },
          { lovelace: 5n * ADA },
        )
        .complete()
    ).sign
      .withWallet()
      .complete(),
  );
  const [daParamsUtxo] = await lucid.utxosByOutRef([
    { txHash: paramsHash, outputIndex: 0 },
  ]);
  const [initUtxo] = await lucid.wallet().getUtxos();
  const init = await Effect.runPromise(
    buildInitDaBondPoolTxProgram(lucid, {
      poolValidator: pool,
      parameters,
      initUtxo: initUtxo!,
      lovelace: parameters.da_bond_pool_floor_lovelace + 600n * ADA,
    }),
  );
  await submit(await (await init.complete()).sign.withWallet().complete());
  const [poolUtxo] = await lucid.utxosAtWithUnit(
    pool.spendingScriptAddress,
    unit,
  );
  const now = BigInt(emulator.now());
  const begin = await Effect.runPromise(
    buildBeginDaBondPoolWithdrawTxProgram(lucid, {
      poolValidator: pool,
      parameters,
      pool: { utxo: poolUtxo! },
      daParamsUtxo: daParamsUtxo!,
      signerKeyHashes: [ownerA.keyHash, ownerB.keyHash],
      withdrawDelayMs: WITHDRAW_DELAY_MS,
      validity: { validFrom: now, validTo: now + 300_000n },
    }),
  );
  const unsigned = await begin.complete();
  return {
    emulator,
    lucid,
    unit,
    poolAddress: pool.spendingScriptAddress,
    feePayer,
    ownerA,
    ownerB,
    stranger,
    txCbor: unsigned.toCBOR(),
    txHash: unsigned.toHash(),
  };
};

const reasonOf = (run: () => unknown): DaBondPoolBuildFailureReason => {
  try {
    run();
  } catch (error) {
    expect(error).toBeInstanceOf(DaBondPoolBuildError);
    return (error as DaBondPoolBuildError).reason;
  }
  throw new Error("expected a DaBondPoolBuildError, nothing was thrown");
};

const errorOf = (run: () => unknown): DaBondPoolBuildError => {
  try {
    run();
  } catch (error) {
    expect(error).toBeInstanceOf(DaBondPoolBuildError);
    return error as DaBondPoolBuildError;
  }
  throw new Error("expected a DaBondPoolBuildError, nothing was thrown");
};

/** The one vkey witness in a witness set, as [vkey hex, signature hex]. */
const vkeyWitnesses = (witnessSetCbor: string): readonly string[][] => {
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const list = witnessSet.vkeywitnesses();
  const out: string[][] = [];
  for (let index = 0; index < (list?.len() ?? 0); index += 1) {
    const witness = list!.get(index);
    out.push([
      Buffer.from(witness.vkey().to_raw_bytes()).toString("hex"),
      witness.ed25519_signature().to_hex(),
    ]);
  }
  return out;
};

/** Rebuilds a one-witness set with the signature's last byte flipped. */
const tamperedSignature = (witnessSetCbor: string): string => {
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const witness = witnessSet.vkeywitnesses()!.get(0);
  const signature = witness.ed25519_signature().to_raw_bytes();
  signature[signature.length - 1] ^= 0x01;
  const list = CML.VkeywitnessList.new();
  list.add(
    CML.Vkeywitness.new(
      witness.vkey(),
      CML.Ed25519Signature.from_raw_bytes(signature),
    ),
  );
  const tampered = CML.TransactionWitnessSet.new();
  tampered.set_vkeywitnesses(list);
  return tampered.to_cbor_hex();
};

/** Every witness-set field except the vkeys. */
const nonVkeyWitnessFields = (txCbor: string): Record<string, unknown> => {
  const fields = JSON.parse(
    CML.Transaction.from_cbor_hex(txCbor).witness_set().to_json(),
  ) as Record<string, unknown>;
  delete fields.vkeywitnesses;
  return fields;
};

describe("DA bond pool owner-quorum primitives", () => {
  let scene: Scene;

  beforeAll(async () => {
    scene = await setupScene();
  });

  it("is exported from the package entry point", () => {
    expect(Sdk.witnessDaBondPoolTx).toBe(witnessDaBondPoolTx);
    expect(Sdk.assembleDaBondPoolTx).toBe(assembleDaBondPoolTx);
    expect(Sdk.assertDaBondPoolWitnessQuorum).toBe(
      assertDaBondPoolWitnessQuorum,
    );
  });

  it("reads the body hash and the required signers off the CBOR", () => {
    expect(daBondPoolTxBodyHash(scene.txCbor)).toBe(scene.txHash);
    expect(daBondPoolTxRequiredSigners(scene.txCbor)).toEqual(
      [scene.ownerA.keyHash, scene.ownerB.keyHash].sort(),
    );
  });

  it("witnesses with one key and verifies the witness back", () => {
    for (const owner of [scene.ownerA, scene.ownerB]) {
      const witness = witnessDaBondPoolTx(scene.txCbor, owner.bech32);
      expect(witness.keyHash).toBe(owner.keyHash);
      expect(vkeyWitnesses(witness.witnessSetCbor)).toHaveLength(1);
      expect(
        verifiedDaBondPoolWitnessKeyHashes(
          scene.txCbor,
          witness.witnessSetCbor,
        ),
      ).toEqual([owner.keyHash]);
    }
    expect(scene.ownerA.bech32.startsWith("ed25519e_sk1")).toBe(true);
    expect(scene.ownerB.bech32.startsWith("ed25519_sk1")).toBe(true);
  });

  it("refuses a tampered signature, a witness over another body, and garbage", () => {
    const witness = witnessDaBondPoolTx(scene.txCbor, scene.ownerA.bech32);
    const tampered = tamperedSignature(witness.witnessSetCbor);
    expect(vkeyWitnesses(tampered)[0]![0]).toBe(
      vkeyWitnesses(witness.witnessSetCbor)[0]![0],
    );
    expect(
      reasonOf(() =>
        verifiedDaBondPoolWitnessKeyHashes(scene.txCbor, tampered),
      ),
    ).toBe("invalid_witness");
    expect(
      reasonOf(() =>
        assertDaBondPoolWitnessQuorum({
          txCbor: scene.txCbor,
          witnessSetCbors: [
            witnessDaBondPoolTx(scene.txCbor, scene.ownerB.bech32)
              .witnessSetCbor,
            tampered,
          ],
          daParams: daParams([scene.ownerA, scene.ownerB], 1n),
        }),
      ),
    ).toBe("invalid_witness");
    expect(reasonOf(() => assembleDaBondPoolTx(scene.txCbor, [tampered]))).toBe(
      "invalid_witness",
    );

    // A signature by the right key over a different body.
    const otherBody = CML.Transaction.from_cbor_hex(scene.txCbor).body();
    otherBody.set_ttl((otherBody.ttl() ?? 0n) + 1n);
    const otherTx = CML.Transaction.new(
      otherBody,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex();
    expect(daBondPoolTxBodyHash(otherTx)).not.toBe(scene.txHash);
    const foreign = witnessDaBondPoolTx(otherTx, scene.ownerA.bech32);
    expect(
      reasonOf(() =>
        verifiedDaBondPoolWitnessKeyHashes(
          scene.txCbor,
          foreign.witnessSetCbor,
        ),
      ),
    ).toBe("invalid_witness");

    for (const garbage of ["", "zz", "82", "a10081"]) {
      expect(
        reasonOf(() =>
          verifiedDaBondPoolWitnessKeyHashes(scene.txCbor, garbage),
        ),
      ).toBe("invalid_witness");
    }
  });

  it("refuses one of two owners with the exact shortfall message", () => {
    expect(daBondPoolQuorumShortfallMessage(1, 2n)).toBe(
      "Refusing to submit: 1 distinct DA params owner witness(es), update_threshold is 2; nothing was submitted",
    );
    // ownerB is also a required signer without a witness: the quorum
    // shortfall is reported first.
    const error = errorOf(() =>
      assertDaBondPoolWitnessQuorum({
        txCbor: scene.txCbor,
        witnessSetCbors: [
          witnessDaBondPoolTx(scene.txCbor, scene.ownerA.bech32).witnessSetCbor,
        ],
        daParams: daParams([scene.ownerA, scene.ownerB], 2n),
      }),
    );
    expect(error.reason).toBe("insufficient_signers");
    expect(error.message).toBe(daBondPoolQuorumShortfallMessage(1, 2n));
  });

  it("counts the same owner witnessed twice once", () => {
    const first = witnessDaBondPoolTx(scene.txCbor, scene.ownerA.bech32);
    const second = witnessDaBondPoolTx(scene.txCbor, scene.ownerA.bech32);
    const error = errorOf(() =>
      assertDaBondPoolWitnessQuorum({
        txCbor: scene.txCbor,
        witnessSetCbors: [first.witnessSetCbor, second.witnessSetCbor],
        daParams: daParams([scene.ownerA, scene.ownerB], 2n),
      }),
    );
    expect(error.reason).toBe("insufficient_signers");
    expect(error.message).toBe(daBondPoolQuorumShortfallMessage(1, 2n));
  });

  it("does not count a non-owner, nor an owner the body does not list", () => {
    const witnesses = [scene.ownerA, scene.stranger].map(
      (key) => witnessDaBondPoolTx(scene.txCbor, key.bech32).witnessSetCbor,
    );
    const nonOwner = errorOf(() =>
      assertDaBondPoolWitnessQuorum({
        txCbor: scene.txCbor,
        witnessSetCbors: witnesses,
        daParams: daParams([scene.ownerA, scene.ownerB], 2n),
      }),
    );
    expect(nonOwner.reason).toBe("insufficient_signers");
    expect(nonOwner.message).toBe(daBondPoolQuorumShortfallMessage(1, 2n));

    // The stranger is an owner here, but the body does not list them as a
    // required signer, so the validator would not count them either.
    const unlisted = errorOf(() =>
      assertDaBondPoolWitnessQuorum({
        txCbor: scene.txCbor,
        witnessSetCbors: witnesses,
        daParams: daParams([scene.ownerA, scene.ownerB, scene.stranger], 2n),
      }),
    );
    expect(unlisted.reason).toBe("insufficient_signers");
    expect(unlisted.message).toBe(daBondPoolQuorumShortfallMessage(1, 2n));
  });

  it("refuses a met threshold when a required signer has no witness", () => {
    const ownerAWitness = witnessDaBondPoolTx(
      scene.txCbor,
      scene.ownerA.bech32,
    ).witnessSetCbor;
    const bodySigner = errorOf(() =>
      assertDaBondPoolWitnessQuorum({
        txCbor: scene.txCbor,
        witnessSetCbors: [ownerAWitness],
        daParams: daParams([scene.ownerA, scene.ownerB], 1n),
      }),
    );
    expect(bodySigner.reason).toBe("missing_witness");
    expect(bodySigner.message).toContain(scene.ownerB.keyHash);
    expect(bodySigner.message).not.toContain(scene.ownerA.keyHash);

    const ownerBWitness = witnessDaBondPoolTx(
      scene.txCbor,
      scene.ownerB.bech32,
    ).witnessSetCbor;
    const feePayer = errorOf(() =>
      assertDaBondPoolWitnessQuorum({
        txCbor: scene.txCbor,
        witnessSetCbors: [ownerAWitness, ownerBWitness],
        daParams: daParams([scene.ownerA, scene.ownerB], 2n),
        requiredKeyHashes: [scene.feePayer.keyHash],
      }),
    );
    expect(feePayer.reason).toBe("missing_witness");
    expect(feePayer.message).toContain(scene.feePayer.keyHash);
  });

  it("assembles a quorum the ledger accepts, keeping the body and the witness fields", async () => {
    const witnessSetCbors = [scene.ownerA, scene.ownerB, scene.feePayer].map(
      (key) => witnessDaBondPoolTx(scene.txCbor, key.bech32).witnessSetCbor,
    );
    const quorum = assertDaBondPoolWitnessQuorum({
      txCbor: scene.txCbor,
      witnessSetCbors,
      daParams: daParams([scene.ownerA, scene.ownerB], 2n),
      requiredKeyHashes: [scene.feePayer.keyHash],
    });
    expect(quorum.ownerKeyHashes).toEqual(
      [scene.ownerA.keyHash, scene.ownerB.keyHash].sort(),
    );
    expect(quorum.witnessKeyHashes).toEqual(
      [
        scene.ownerA.keyHash,
        scene.ownerB.keyHash,
        scene.feePayer.keyHash,
      ].sort(),
    );

    // The same owner's witness twice lands once.
    const assembled = assembleDaBondPoolTx(scene.txCbor, [
      ...witnessSetCbors,
      witnessSetCbors[0]!,
    ]);
    expect(daBondPoolTxBodyHash(assembled)).toBe(scene.txHash);
    expect(CML.Transaction.from_cbor_hex(assembled).body().to_cbor_hex()).toBe(
      CML.Transaction.from_cbor_hex(scene.txCbor).body().to_cbor_hex(),
    );
    const before = nonVkeyWitnessFields(scene.txCbor);
    expect(before.redeemers).toBeTruthy();
    expect(before.plutus_v3_scripts).toBeTruthy();
    expect(nonVkeyWitnessFields(assembled)).toEqual(before);
    const vkeys = CML.Transaction.from_cbor_hex(assembled)
      .witness_set()
      .vkeywitnesses();
    expect(vkeys?.len()).toBe(3);

    const txHash = await scene.lucid.wallet().submitTx(assembled);
    expect(txHash).toBe(scene.txHash);
    await scene.lucid.awaitTx(txHash);
    scene.emulator.awaitBlock(1);
    const [pool] = await scene.lucid.utxosAtWithUnit(
      scene.poolAddress,
      scene.unit,
    );
    expect(pool!.txHash).toBe(txHash);
    expect(decodeDaBondPoolDatum(pool!.datum!)).toMatchObject({
      Withdrawing: {},
    });
  });

  it("keeps plutus data and existing vkeys already in the witness set", () => {
    const tx = CML.Transaction.from_cbor_hex(scene.txCbor);
    const witnessSet = tx.witness_set();
    const datums = CML.PlutusDataList.new();
    datums.add(CML.PlutusData.from_cbor_hex(Data.to(42n)));
    witnessSet.set_plutus_datums(datums);
    const existing = witnessDaBondPoolTx(scene.txCbor, scene.ownerA.bech32);
    witnessSet.set_vkeywitnesses(
      CML.TransactionWitnessSet.from_cbor_hex(
        existing.witnessSetCbor,
      ).vkeywitnesses()!,
    );
    const withDatum = CML.Transaction.new(
      tx.body(),
      witnessSet,
      true,
    ).to_cbor_hex();
    expect(daBondPoolTxBodyHash(withDatum)).toBe(scene.txHash);

    const ownerB = witnessDaBondPoolTx(
      scene.txCbor,
      scene.ownerB.bech32,
    ).witnessSetCbor;
    // Only ownerB's witness is passed: ownerA's must come from the
    // transaction's own witness set.
    const assembled = assembleDaBondPoolTx(withDatum, [ownerB]);
    expect(daBondPoolTxBodyHash(assembled)).toBe(scene.txHash);
    const after = nonVkeyWitnessFields(assembled);
    expect(after.plutus_datums).toBeTruthy();
    expect(after).toEqual(nonVkeyWitnessFields(withDatum));
    const vkeysOf = (txCbor: string) =>
      CML.Transaction.from_cbor_hex(txCbor)
        .witness_set()
        .vkeywitnesses()
        ?.len();
    expect(
      [
        ...verifiedDaBondPoolWitnessKeyHashes(
          scene.txCbor,
          CML.Transaction.from_cbor_hex(assembled).witness_set().to_cbor_hex(),
        ),
      ].sort(),
    ).toEqual([scene.ownerA.keyHash, scene.ownerB.keyHash].sort());
    expect(vkeysOf(assembled)).toBe(2);
    // A witness passed again beside the one already present is kept once.
    expect(
      vkeysOf(
        assembleDaBondPoolTx(withDatum, [existing.witnessSetCbor, ownerB]),
      ),
    ).toBe(2);
  });
});
