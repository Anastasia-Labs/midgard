/** The funding permit admits a thread reclaim only as the exact cancel of
 * the action's own thread back to our wallet: one thread spent, its one
 * token burned, every output ours. Anything wider is refused before a
 * byte is submitted. */
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { threadReclaimAction } from "../src/workflow/own-thread-reclaim.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { fundingAddress } from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const TOKEN_POLICY = "e1".repeat(28);
const TOKEN_NAME = "aa";
const TOKEN = `${TOKEN_POLICY}${TOKEN_NAME}`;
const THREAD_LOVELACE = 3_000_000n;
const FUNDING_LOVELACE = 15_000_000n;
const FEE = 200_000n;

const thread = (address: string, txHash: string): UTxO => ({
  txHash,
  outputIndex: 0,
  address,
  assets: { lovelace: THREAD_LOVELACE, [TOKEN]: 1n },
  datum: "00",
});

const burn = (entries: readonly (readonly [string, bigint])[]) => {
  const mint = CML.Mint.new();
  for (const [name, quantity] of entries)
    mint.set(
      CML.ScriptHash.from_hex(TOKEN_POLICY),
      CML.AssetName.from_hex(name),
      quantity,
    );
  return mint;
};

const setup = async () => {
  const sample = await runtimeFunding("step-one");
  const address = readWorkflowRuntimeFundingPolicy(sample.policy).contracts[0]!
    .address;
  const ours = thread(address, "91".repeat(32));
  const theirs = thread(address, "92".repeat(32));
  const ourRef = `${ours.txHash}#0`;
  const action = threadReclaimAction({
    threadOutRef: ourRef,
    step: 2,
    lovelace: THREAD_LOVELACE.toString(),
  });
  const runtime = await runtimeFunding("step-one", {
    action,
    additionalInputs: [ours, theirs],
    confirmedInputs: [ours, theirs],
  });
  const prepare = (signed: ReturnType<typeof signedFundingTransaction>) =>
    runtime.prepareTransaction({
      action,
      preflight: bindWorkflowPreflightTransaction(
        Object.freeze({ txHash: signed.toHash() }),
        signed,
      ),
    });
  return { runtime, address, ours, theirs, ourRef, prepare };
};

describe("funding permit: thread reclaim", () => {
  it("admits the exact cancel of our own thread back to our wallet", async () => {
    const { runtime, ourRef, prepare } = await setup();
    await expect(
      prepare(
        signedFundingTransaction({
          inputOutRefs: [...runtime.selected.fundingOutRefs, ourRef],
          outputLovelace: FUNDING_LOVELACE + THREAD_LOVELACE - FEE,
          fee: FEE,
          mint: burn([[TOKEN_NAME, -1n]]),
        }),
      ),
    ).resolves.toBeUndefined();
  });

  it("refuses a reclaim that spends another thread, or one more besides ours", async () => {
    for (const spent of ["theirs", "both"] as const) {
      const { runtime, ourRef, theirs, prepare } = await setup();
      const theirRef = `${theirs.txHash}#0`;
      await expect(
        prepare(
          signedFundingTransaction({
            inputOutRefs: [
              ...runtime.selected.fundingOutRefs,
              ...(spent === "theirs" ? [theirRef] : [ourRef, theirRef]),
            ],
            outputLovelace:
              FUNDING_LOVELACE +
              THREAD_LOVELACE * (spent === "theirs" ? 1n : 2n) -
              FEE,
            fee: FEE,
            mint: burn([[TOKEN_NAME, spent === "theirs" ? -1n : -2n]]),
          }),
        ),
      ).rejects.toThrow(
        "thread reclaim differs from its exact binding: spends exactly its own thread",
      );
    }
  });

  it("refuses a reclaim that pays anywhere but our wallet", async () => {
    const { runtime, address, ourRef, prepare } = await setup();
    const datum = () =>
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("00"));
    const elsewhere = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n),
      datum(),
    );
    await expect(
      prepare(
        signedFundingTransaction({
          inputOutRefs: [...runtime.selected.fundingOutRefs, ourRef],
          outputLovelace: FUNDING_LOVELACE + THREAD_LOVELACE - FEE - 2_000_000n,
          fee: FEE,
          mint: burn([[TOKEN_NAME, -1n]]),
          additionalOutputs: [elsewhere],
        }),
      ),
    ).rejects.toThrow(
      "thread reclaim differs from its exact binding: pays only our wallet",
    );
  });

  it("refuses a reclaim that mints or burns anything beyond the thread's one token", async () => {
    for (const mint of [
      [],
      [
        [TOKEN_NAME, -1n],
        ["bb", 1n],
      ],
    ] as const) {
      const { runtime, ourRef, prepare } = await setup();
      await expect(
        prepare(
          signedFundingTransaction({
            inputOutRefs: [...runtime.selected.fundingOutRefs, ourRef],
            outputLovelace: FUNDING_LOVELACE + THREAD_LOVELACE - FEE,
            fee: FEE,
            mint: burn(mint),
          }),
        ),
      ).rejects.toThrow(
        "thread reclaim differs from its exact binding: burns exactly the thread's token",
      );
    }
  });

  it("refuses a reclaim whose wallet output is not ours even with every other binding exact", async () => {
    const { runtime, ourRef, prepare } = await setup();
    const stranger = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex("ab".repeat(28))),
    )
      .to_address()
      .to_bech32();
    expect(stranger).not.toBe(fundingAddress);
    await expect(
      prepare(
        signedFundingTransaction({
          inputOutRefs: [...runtime.selected.fundingOutRefs, ourRef],
          outputLovelace: FUNDING_LOVELACE + THREAD_LOVELACE - FEE,
          outputAddress: stranger,
          fee: FEE,
          mint: burn([[TOKEN_NAME, -1n]]),
        }),
      ),
    ).rejects.toThrow(
      "thread reclaim differs from its exact binding: pays only our wallet",
    );
  });
});
