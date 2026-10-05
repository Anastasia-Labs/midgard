import * as SDK from "@al-ft/midgard-sdk";
import * as Lucid from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { io, PARAMETERS } from "./concurrent-challenges.fixture.js";

/** Actual SDK signing/inspection over CML; provider and ledger I/O are controlled. */
export const completionLedger = async (oldCost = 8620n, newCost = 4310n) => {
  const sdk = await vi.importActual<typeof SDK>("@al-ft/midgard-sdk");
  const lucid = await vi.importActual<typeof Lucid>("@lucid-evolution/lucid");
  const protocol = await new lucid.Emulator([]).getProtocolParameters();
  const initial = { ...protocol, coinsPerUtxoByte: oldCost };
  let canonical = initial;
  const refreshed = {
    ...protocol,
    coinsPerUtxoByte: newCost,
    maxTxSize: protocol.maxTxSize - 1,
    maxTxExMem: protocol.maxTxExMem - 1n,
    maxTxExSteps: protocol.maxTxExSteps - 1n,
  };
  const key = lucid.CML.PrivateKey.generate_ed25519();
  const address = lucid.credentialToAddress("Custom", {
    type: "Key",
    hash: key.to_public().hash().to_hex(),
  });
  const actor = key.to_public().hash().to_hex();
  io.walletAddress = actor;
  io.queuePolicy = "a8".repeat(28);
  io.limits = sdk.daAvailabilityOperationLimits;
  io.minimumAda = (cost, input) =>
    lucid.calculateMinLovelaceFromUTxO(cost, {
      ...input,
      address: input.address === actor ? address : input.address,
    });
  let onAllocate:
    | ((lucid: Lucid.LucidEvolution, index: number) => void)
    | undefined;
  const instances: {
    lucid: Lucid.LucidEvolution;
    refresh: ReturnType<typeof vi.fn>;
    walletRead: ReturnType<typeof vi.fn>;
  }[] = [];
  io.lucidAllocated = (instance) => {
    const config = instance.config();
    let parameters = initial;
    const wallet = instance.wallet();
    const walletRead = vi.fn(wallet.getUtxos);
    const refresh = vi.fn(async () => {
      if (refresh.mock.calls.length > 1) canonical = refreshed;
      parameters = canonical;
    });
    Object.assign(instance, {
      config: () => ({ ...config, protocolParameters: parameters }),
      wallet: () => ({ ...wallet, getUtxos: walletRead }),
      switchProvider: refresh,
    });
    instances.push({ lucid: instance, refresh, walletRead });
    onAllocate?.(instance, instances.length - 1);
  };
  io.operation.mockResolvedValue({ status: "unspent", currentSlot: 1 });
  io.submitTx.mockImplementation(async (cbor: string) =>
    lucid.CML.hash_transaction(
      lucid.CML.Transaction.from_cbor_hex(cbor).body(),
    ).to_hex(),
  );
  io.run.mockImplementation(sdk.runDaAvailabilityOperation);
  let signs = 0;
  const minimum = (cost: bigint) =>
    lucid.calculateMinLovelaceFromUTxO(cost, {
      address,
      assets: { lovelace: 2_000_000n },
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
  const transaction = (
    input: { cost?: bigint; fee?: bigint; burnHeader?: string } = {},
  ) => {
    const inputs = lucid.CML.TransactionInputList.new();
    inputs.add(
      lucid.CML.TransactionInput.new(
        lucid.CML.TransactionHash.from_hex("fe".repeat(32)),
        0n,
      ),
    );
    const outputs = lucid.CML.TransactionOutputList.new();
    outputs.add(
      lucid.CML.TransactionOutput.new(
        lucid.CML.Address.from_bech32(address),
        lucid.CML.Value.from_coin(minimum(input.cost ?? newCost)),
      ),
    );
    const body = lucid.CML.TransactionBody.new(
      inputs,
      outputs,
      input.fee ?? 200_000n,
    );
    body.set_validity_interval_start(0n);
    body.set_ttl(1000n);
    const collateral = lucid.CML.TransactionInputList.new();
    collateral.add(
      lucid.CML.TransactionInput.new(
        lucid.CML.TransactionHash.from_hex("fd".repeat(32)),
        0n,
      ),
    );
    body.set_collateral_inputs(collateral);
    if (input.burnHeader !== undefined) {
      const mint = lucid.CML.Mint.new();
      mint.set(
        lucid.CML.ScriptHash.from_hex(io.queuePolicy),
        lucid.CML.AssetName.from_hex(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + input.burnHeader,
        ),
        -1n,
      );
      body.set_mint(mint);
    }
    const witnesses = () => {
      const result = lucid.CML.TransactionWitnessSet.new();
      const redeemers = lucid.CML.LegacyRedeemerList.new();
      redeemers.add(
        lucid.CML.LegacyRedeemer.new(
          lucid.CML.RedeemerTag.Spend,
          0n,
          lucid.CML.PlutusData.new_integer(lucid.CML.BigInteger.from_str("0")),
          lucid.CML.ExUnits.new(1_000_000n, 100_000_000n),
        ),
      );
      result.set_redeemers(
        lucid.CML.Redeemers.new_arr_legacy_redeemer(redeemers),
      );
      return result;
    };
    const unsigned = lucid.CML.Transaction.new(body, witnesses(), true);
    return {
      toTransaction: () => unsigned,
      sign: {
        withWallet: () => ({
          complete: async () => {
            signs += 1;
            const signedWitnesses = witnesses();
            const vkeys = lucid.CML.VkeywitnessList.new();
            vkeys.add(
              lucid.CML.make_vkey_witness(
                lucid.CML.hash_transaction(body),
                key,
              ),
            );
            signedWitnesses.set_vkeywitnesses(vkeys);
            return {
              toCBOR: () =>
                lucid.CML.Transaction.new(
                  body,
                  signedWitnesses,
                  true,
                ).to_cbor_hex(),
            };
          },
        }),
      },
    } as unknown as Lucid.TxSignBuilder;
  };
  return {
    onAllocate: (callback: NonNullable<typeof onAllocate>) => {
      onAllocate = callback;
    },
    sdk,
    lucid,
    actor,
    setCanonical: (parameters: typeof initial) => {
      canonical = parameters;
    },
    initial,
    refreshed,
    instances,
    transaction,
    minimum,
    signs: () => signs,
    limits: (instance: Lucid.LucidEvolution) =>
      sdk.daAvailabilityOperationLimits(instance, PARAMETERS),
  };
};

export const deferred = <T>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};
