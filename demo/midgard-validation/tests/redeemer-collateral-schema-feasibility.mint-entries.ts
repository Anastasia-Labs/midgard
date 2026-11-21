import { readFileSync } from "node:fs";

import { CML } from "@lucid-evolution/lucid";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly BlueprintValidator[];
};

export const alwaysSucceedsCompiledCode =
  alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === "midgard.deposit_spend.else",
  )?.compiledCode;

export const optionalCborHex = (
  value: { readonly to_cbor_bytes: () => Uint8Array } | undefined,
): string | undefined =>
  value === undefined
    ? undefined
    : Buffer.from(value.to_cbor_bytes()).toString("hex");

const collectionItemCborHexes = (collection: {
  readonly len: () => number;
  readonly get: (index: number) => { readonly to_cbor_bytes: () => Uint8Array };
}): readonly string[] =>
  Array.from({ length: collection.len() }, (_, index) =>
    Buffer.from(collection.get(index).to_cbor_bytes()).toString("hex"),
  );

export const optionalCollectionItemCborHexes = (
  collection:
    | {
        readonly len: () => number;
        readonly get: (index: number) => {
          readonly to_cbor_bytes: () => Uint8Array;
        };
      }
    | undefined,
): readonly string[] | undefined =>
  collection === undefined ? undefined : collectionItemCborHexes(collection);

const optionalKeyHashHexes = (
  collection: CML.Ed25519KeyHashList | undefined,
): readonly string[] | undefined =>
  collection === undefined
    ? undefined
    : Array.from({ length: collection.len() }, (_, index) =>
        collection.get(index).to_hex(),
      );

const withdrawalEntries = (
  withdrawals: CML.MapRewardAccountToCoin | undefined,
): readonly {
  readonly rewardAccountHex: string;
  readonly amount: bigint;
}[] => {
  if (withdrawals === undefined) {
    return [];
  }
  const keys = withdrawals.keys();
  return Array.from({ length: keys.len() }, (_, index) => {
    const rewardAccount = keys.get(index);
    const amount = withdrawals.get(rewardAccount);
    if (amount === undefined) {
      throw new Error("Withdrawal entry has no amount");
    }
    return {
      rewardAccountHex: Buffer.from(
        rewardAccount.to_address().to_raw_bytes(),
      ).toString("hex"),
      amount,
    };
  }).sort((left, right) =>
    left.rewardAccountHex.localeCompare(right.rewardAccountHex),
  );
};

const mintEntries = (
  mint: CML.Mint | undefined,
): readonly {
  readonly policyIdHex: string;
  readonly assets: readonly {
    readonly assetNameHex: string;
    readonly quantity: bigint;
  }[];
}[] => {
  if (mint === undefined) {
    return [];
  }
  const policies = mint.keys();
  return Array.from({ length: policies.len() }, (_, policyIndex) => {
    const policy = policies.get(policyIndex);
    const policyAssets = mint.get_assets(policy);
    if (policyAssets === undefined) {
      throw new Error("Mint policy has no asset map");
    }
    const assetNames = policyAssets.keys();
    const assets = Array.from({ length: assetNames.len() }, (_, assetIndex) => {
      const assetName = assetNames.get(assetIndex);
      const quantity = policyAssets.get(assetName);
      if (quantity === undefined) {
        throw new Error("Mint asset has no quantity");
      }
      return {
        assetNameHex: assetName.to_hex(),
        quantity,
      };
    }).sort((left, right) =>
      left.assetNameHex.localeCompare(right.assetNameHex),
    );
    return {
      policyIdHex: policy.to_hex(),
      assets,
    };
  }).sort((left, right) => left.policyIdHex.localeCompare(right.policyIdHex));
};

export const sharedBodyFields = (
  body: CML.TransactionBody,
): {
  readonly spendInputs: readonly string[];
  readonly referenceInputs: readonly string[] | undefined;
  readonly outputs: readonly string[];
  readonly fee: bigint;
  readonly validityStart: bigint | undefined;
  readonly ttl: bigint | undefined;
  readonly withdrawals: readonly {
    readonly rewardAccountHex: string;
    readonly amount: bigint;
  }[];
  readonly requiredSigners: readonly string[] | undefined;
  readonly mint: readonly {
    readonly policyIdHex: string;
    readonly assets: readonly {
      readonly assetNameHex: string;
      readonly quantity: bigint;
    }[];
  }[];
  readonly scriptDataHash: string | undefined;
  readonly auxiliaryDataHash: string | undefined;
  readonly networkId: bigint | undefined;
} => ({
  spendInputs: collectionItemCborHexes(body.inputs()),
  referenceInputs: optionalCollectionItemCborHexes(body.reference_inputs()),
  outputs: collectionItemCborHexes(body.outputs()),
  fee: body.fee(),
  validityStart: body.validity_interval_start(),
  ttl: body.ttl(),
  withdrawals: withdrawalEntries(body.withdrawals()),
  requiredSigners: optionalKeyHashHexes(body.required_signers()),
  mint: mintEntries(body.mint()),
  scriptDataHash: body.script_data_hash()?.to_hex(),
  auxiliaryDataHash: body.auxiliary_data_hash()?.to_hex(),
  networkId: body.network_id()?.network(),
});
