import { planMidgardFieldCarriage } from "@al-ft/midgard-core";
import * as sdk from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Lucid,
  makeTxSignBuilder,
  type UTxO,
  type Wallet,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import {
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";

const address = credentialToAddress("Custom", {
  type: "Key",
  hash: "12".repeat(28),
});
const publications = [0, 1].map((chunkIndex) => ({
  chunkIndex,
  bytes: Buffer.from([chunkIndex]),
  digest: Buffer.alloc(32, chunkIndex),
}));
const planned: FaultProofFieldOpeningPlan = {
  sourceKind: 0n,
  fieldIndex: 0,
  nativeTxId: "00".repeat(32),
  nativeTxCompactCbor: "80",
  preimage: Buffer.from([0]),
  itemCount: 1,
  commitment: "00".repeat(32),
  plan: {
    ...planMidgardFieldCarriage({
      owner: Buffer.alloc(28),
      txId: Buffer.alloc(32),
      fieldIndex: 0,
      preimage: Buffer.from([0]),
      publish: true,
    }),
    publications,
  },
};

const unexpectedWalletCall = (): never => {
  throw new Error("unexpected wallet call in publication boundary test");
};
const wallet: Wallet = {
  overrideUTxOs: unexpectedWalletCall,
  address: unexpectedWalletCall,
  rewardAddress: unexpectedWalletCall,
  getUtxos: unexpectedWalletCall,
  getUtxosCore: unexpectedWalletCall,
  getDelegation: unexpectedWalletCall,
  signTx: unexpectedWalletCall,
  signMessage: unexpectedWalletCall,
  submitTx: unexpectedWalletCall,
};
afterEach(() => vi.restoreAllMocks());

it.each([false, true])(
  "confirms each publication before the next build, including a retained first chunk (%s)",
  async (retained) => {
    const events: string[] = [];
    const outputs: UTxO[] = [];
    const firstDatum = sdk.fieldPreimagePublicationDatumCbor(
      publications[0]!.bytes,
    );
    if (retained)
      outputs.push({
        txHash: "34".repeat(32),
        outputIndex: 0,
        address,
        assets: { lovelace: 2_000_000n },
        datum: firstDatum,
      });
    const build = vi
      .spyOn(sdk, "buildUnsignedFieldPreimagePublicationProgram")
      .mockImplementation((_lucid, { publication }) =>
        Effect.promise(async () => {
          const index = publication.chunkIndex;
          events.push(`build:${index}`);
          const cmlOutputs = CML.TransactionOutputList.new();
          cmlOutputs.add(
            CML.TransactionOutput.new(
              CML.Address.from_bech32(address),
              CML.Value.from_coin(2_000_000n),
              CML.DatumOption.new_datum(
                CML.PlutusData.from_cbor_hex(publication.datumCbor),
              ),
            ),
          );
          const body = CML.TransactionBody.new(
            CML.TransactionInputList.new(),
            cmlOutputs,
            0n,
          );
          const transaction = CML.Transaction.new(
            body,
            CML.TransactionWitnessSet.new(),
            true,
            undefined,
          );
          const txHash = CML.hash_transaction(body).to_hex();
          const builder = makeTxSignBuilder(wallet, transaction);
          const signed = await builder.complete();
          vi.spyOn(signed, "submit").mockImplementation(async () => {
            events.push(`submit:${index}`);
            outputs.push({
              txHash,
              outputIndex: 0,
              address,
              assets: { lovelace: 2_000_000n },
              datum: publication.datumCbor,
            });
            return txHash;
          });
          vi.spyOn(builder.sign, "withWallet").mockReturnValue(builder);
          vi.spyOn(builder, "complete").mockResolvedValue(signed);
          return builder;
        }),
      );
    const lucid = await Lucid(undefined, "Custom", {
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
    });
    vi.spyOn(lucid, "utxosAt").mockImplementation(async () => [...outputs]);
    vi.spyOn(lucid, "utxosByOutRef").mockImplementation(async (refs) =>
      outputs.filter((output) =>
        refs.some(
          (ref) =>
            ref.txHash === output.txHash &&
            ref.outputIndex === output.outputIndex,
        ),
      ),
    );
    vi.spyOn(lucid, "awaitTx").mockImplementation(async (txHash) => {
      events.push(
        `included:${outputs.findIndex((output) => output.txHash === txHash)}`,
      );
      return true;
    });
    const result = await publishFaultProofFieldCarriage({
      lucid,
      signer: {
        source: "test",
        address,
        paymentKeyHash: "12".repeat(28),
        selectWallet: () => {},
      },
      planned,
      publisherAddress: address,
      label: "publication lifecycle",
      beforePublication: async () => {
        events.push("begin");
      },
      preSubmitBoundary: async ({ txHash }) => {
        events.push(`persist:${outputs.length}`);
        expect(txHash).toMatch(/^[0-9a-f]{64}$/u);
      },
      publicationConfirmed: async (txHash) => {
        events.push(
          `confirm:${outputs.findIndex((output) => output.txHash === txHash)}`,
        );
      },
    });
    expect(build).toHaveBeenCalledTimes(retained ? 1 : 2);
    expect(result).toHaveLength(2);
    expect(events).toEqual([
      ...(retained
        ? ["confirm:0"]
        : [
            "begin",
            "build:0",
            "persist:0",
            "submit:0",
            "included:0",
            "confirm:0",
          ]),
      "begin",
      "build:1",
      "persist:1",
      "submit:1",
      "included:1",
      "confirm:1",
    ]);
  },
);
