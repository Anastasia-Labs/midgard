import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
} from "@al-ft/midgard-core/codec/cbor";
import type { MintAuthorizationMintScanControl } from "@al-ft/midgard-sdk";

export const initialMintScan = (
  bytes: Buffer,
  policyIndex: bigint,
): MintAuthorizationMintScanControl => {
  const header = readCborArrayHeader(bytes, 0);
  if (
    bytes.length > 32768 ||
    policyIndex < 0n ||
    policyIndex >= BigInt(header.length)
  )
    throw new Error(
      "mint scan policy coordinate is outside the committed field",
    );
  return {
    item_count: BigInt(header.length),
    item_index: 0n,
    cursor: BigInt(header.nextOffset),
    item_end: 0n,
    remaining_assets: 0n,
    previous_asset_name: null,
    policy_id: "",
    selected_complete: false,
  };
};

export const mintScanComplete = (
  state: MintAuthorizationMintScanControl,
  length: number,
): boolean =>
  state.item_index === state.item_count &&
  state.cursor === BigInt(length) &&
  state.remaining_assets === 0n &&
  state.selected_complete &&
  state.policy_id.length === 56;

export const advanceMintScan = (
  state: MintAuthorizationMintScanControl,
  bytes: Buffer,
  policyIndex: bigint,
): MintAuthorizationMintScanControl => {
  let next = { ...state };
  for (
    let count = 0;
    count < 32 && next.item_index < next.item_count;
    count++
  ) {
    if (next.remaining_assets > 0n) {
      const asset = readCborBytes(bytes, Number(next.cursor));
      const previous =
        next.previous_asset_name === null
          ? null
          : Buffer.from(next.previous_asset_name, "hex");
      if (
        asset.value.length > 32 ||
        (previous !== null &&
          (previous.length > asset.value.length ||
            (previous.length === asset.value.length &&
              Buffer.compare(previous, asset.value) >= 0)))
      )
        throw new Error("mint scan asset names are not canonical");
      const quantity = readCborInteger(bytes, asset.nextOffset);
      if (quantity.value === 0n || BigInt(quantity.nextOffset) > next.item_end)
        throw new Error(
          "mint scan quantity is zero or crosses the policy item",
        );
      const left = next.remaining_assets - 1n;
      if (left === 0n && BigInt(quantity.nextOffset) !== next.item_end)
        throw new Error("mint scan selected policy has trailing bytes");
      next = {
        ...next,
        cursor: BigInt(quantity.nextOffset),
        remaining_assets: left,
        previous_asset_name: asset.value.toString("hex"),
        item_index: left === 0n ? next.item_index + 1n : next.item_index,
        selected_complete: left === 0n,
      };
    } else {
      const item = readCborBytes(bytes, Number(next.cursor));
      if (next.item_index === policyIndex) {
        const start = item.nextOffset - item.value.length;
        if (next.selected_complete || bytes[start] !== 0x82)
          throw new Error("mint scan selected policy is not a canonical pair");
        const policy = readCborBytes(bytes, start + 1);
        const assets = readCborMapHeader(bytes, policy.nextOffset);
        if (
          policy.value.length !== 28 ||
          assets.length === 0 ||
          assets.nextOffset >= item.nextOffset
        )
          throw new Error(
            "mint scan selected policy has no nonempty asset map",
          );
        next = {
          ...next,
          cursor: BigInt(assets.nextOffset),
          item_end: BigInt(item.nextOffset),
          remaining_assets: BigInt(assets.length),
          previous_asset_name: null,
          policy_id: policy.value.toString("hex"),
        };
      } else {
        next = {
          ...next,
          cursor: BigInt(item.nextOffset),
          item_index: next.item_index + 1n,
        };
      }
    }
  }
  if (
    next.item_index === next.item_count &&
    !mintScanComplete(next, bytes.length)
  )
    throw new Error(
      "mint scan envelope count/end or selected policy is incomplete",
    );
  return next;
};
