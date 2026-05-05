import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type Assets,
  type Credential,
  type LucidEvolution,
  type Network,
  type OutputDatum,
  type Script,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";
import {
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
  withStubbedProviderEvaluation,
} from "@/cml-redeemers.js";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  compareOutRefs,
  dedupeByOutRef,
  findOutRefIndex,
  outRefLabel,
  type IndexedTxOutput,
  type OutRefLike,
} from "@/tx-context.js";
import {
  fetchReferenceScriptUtxosProgram,
  type ReferenceScriptResolved,
} from "@/transactions/reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

export class ReservePayoutTxError extends EffectData.TaggedError(
  "ReservePayoutTxError",
)<{
  message: string;
  cause: unknown;
}> {}

export type MembershipProofWithdrawalWitness = {
  readonly script: Script;
  readonly amount?: bigint;
};

export type ReservePayoutReferenceScripts = {
  readonly depositMinting?: UTxO;
  readonly depositSpending?: UTxO;
  readonly depositWitnessCertificate?: UTxO;
  readonly withdrawalMinting?: UTxO;
  readonly withdrawalSpending?: UTxO;
  readonly withdrawalWitnessCertificate?: UTxO;
  readonly membershipProofWithdrawal?: UTxO;
  readonly reserveSpending?: UTxO;
  readonly payoutSpending?: UTxO;
  readonly payoutMinting?: UTxO;
};

type CommonBuilderConfig = {
  readonly hubOracleRefInput?: UTxO;
  readonly feeInput?: UTxO;
  readonly referenceScripts?: ReservePayoutReferenceScripts;
  readonly referenceScriptsAddress?: string;
};

export type AbsorbConfirmedDepositConfig = CommonBuilderConfig & {
  readonly deposit: SDK.DepositUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.Proof;
  readonly membershipProofWithdrawal: MembershipProofWithdrawalWitness;
};

export type InitializePayoutConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.Proof;
  readonly membershipProofWithdrawal: MembershipProofWithdrawalWitness;
};

export type AddReserveFundsConfig = CommonBuilderConfig & {
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
};

export type ConcludePayoutConfig = CommonBuilderConfig & {
  readonly payoutInput: UTxO;
};

export type RefundInvalidWithdrawalConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.Proof;
  readonly membershipProofWithdrawal: MembershipProofWithdrawalWitness;
  readonly validityOverride: Exclude<
    SDK.WithdrawalValidity,
    "WithdrawalIsValid"
  >;
};

export type BuiltReservePayoutTx<L> = {
  readonly tx: TxSignBuilder;
  readonly layout: L;
};

type CompleteWithLayoutParams<L> = {
  readonly label: string;
  readonly lucid: LucidEvolution;
  readonly initialLayout: L;
  readonly walletInputExclusions?: readonly OutRefLike[];
  readonly makeTx: (layout: L) => TxBuilder;
  readonly deriveLayout: (tx: CML.Transaction) => L;
  readonly sameLayout: (left: L, right: L) => boolean;
};

const ADA_POLICY_ID = "";
const ADA_ASSET_NAME = "";
const LOVELACE_UNIT = "lovelace";

const formatLayout = (layout: unknown): string =>
  JSON.stringify(layout, (_key, value) =>
    typeof value === "bigint" ? value.toString() : value,
  );

type CborNode =
  | { readonly kind: "uint"; readonly value: bigint }
  | { readonly kind: "nint"; readonly value: bigint }
  | { readonly kind: "bytes"; readonly value: Buffer }
  | {
      readonly kind: "array";
      readonly items: readonly CborNode[];
      readonly indefinite: boolean;
    }
  | {
      readonly kind: "map";
      readonly entries: readonly (readonly [CborNode, CborNode])[];
    }
  | { readonly kind: "tag"; readonly tag: bigint; readonly value: CborNode };

const readCborLength = (
  bytes: Buffer,
  offset: number,
  additional: number,
): { readonly value: bigint | null; readonly offset: number } => {
  if (additional < 24) {
    return { value: BigInt(additional), offset };
  }
  if (additional === 24) {
    return { value: BigInt(bytes[offset]!), offset: offset + 1 };
  }
  if (additional === 25) {
    return { value: BigInt(bytes.readUInt16BE(offset)), offset: offset + 2 };
  }
  if (additional === 26) {
    return { value: BigInt(bytes.readUInt32BE(offset)), offset: offset + 4 };
  }
  if (additional === 27) {
    return { value: bytes.readBigUInt64BE(offset), offset: offset + 8 };
  }
  if (additional === 31) {
    return { value: null, offset };
  }
  throw new Error(`Unsupported CBOR additional information ${additional}`);
};

const expectCborLength = (length: bigint | null, context: string): bigint => {
  if (length === null) {
    throw new Error(`${context} must use a definite length`);
  }
  return length;
};

const parseCborNode = (
  bytes: Buffer,
  offset: number,
): { readonly node: CborNode; readonly offset: number } => {
  const initial = bytes[offset];
  if (initial === undefined) {
    throw new Error("Unexpected end of CBOR input");
  }
  if (initial === 0xff) {
    throw new Error("Unexpected CBOR break marker");
  }

  const major = initial >> 5;
  const additional = initial & 0x1f;
  const length = readCborLength(bytes, offset + 1, additional);

  if (major === 0) {
    return {
      node: { kind: "uint", value: expectCborLength(length.value, "uint") },
      offset: length.offset,
    };
  }
  if (major === 1) {
    return {
      node: { kind: "nint", value: expectCborLength(length.value, "nint") },
      offset: length.offset,
    };
  }
  if (major === 2) {
    const byteLength = Number(expectCborLength(length.value, "bytes"));
    const end = length.offset + byteLength;
    return {
      node: { kind: "bytes", value: bytes.subarray(length.offset, end) },
      offset: end,
    };
  }
  if (major === 4) {
    const items: CborNode[] = [];
    if (length.value === null) {
      let cursor = length.offset;
      while (bytes[cursor] !== 0xff) {
        const parsed = parseCborNode(bytes, cursor);
        items.push(parsed.node);
        cursor = parsed.offset;
      }
      return {
        node: { kind: "array", items, indefinite: true },
        offset: cursor + 1,
      };
    }
    let cursor = length.offset;
    for (let i = 0n; i < length.value; i += 1n) {
      const parsed = parseCborNode(bytes, cursor);
      items.push(parsed.node);
      cursor = parsed.offset;
    }
    return {
      node: { kind: "array", items, indefinite: false },
      offset: cursor,
    };
  }
  if (major === 5) {
    const entries: (readonly [CborNode, CborNode])[] = [];
    if (length.value === null) {
      let cursor = length.offset;
      while (bytes[cursor] !== 0xff) {
        const key = parseCborNode(bytes, cursor);
        const value = parseCborNode(bytes, key.offset);
        entries.push([key.node, value.node]);
        cursor = value.offset;
      }
      return { node: { kind: "map", entries }, offset: cursor + 1 };
    }
    let cursor = length.offset;
    for (let i = 0n; i < length.value; i += 1n) {
      const key = parseCborNode(bytes, cursor);
      const value = parseCborNode(bytes, key.offset);
      entries.push([key.node, value.node]);
      cursor = value.offset;
    }
    return { node: { kind: "map", entries }, offset: cursor };
  }
  if (major === 6) {
    const parsed = parseCborNode(bytes, length.offset);
    return {
      node: {
        kind: "tag",
        tag: expectCborLength(length.value, "tag"),
        value: parsed.node,
      },
      offset: parsed.offset,
    };
  }

  throw new Error(`Unsupported PlutusData CBOR major type ${major}`);
};

const encodeCborHeader = (major: number, value: bigint | null): Buffer => {
  if (value === null) {
    return Buffer.from([(major << 5) | 31]);
  }
  if (value < 24n) {
    return Buffer.from([(major << 5) | Number(value)]);
  }
  if (value <= 0xffn) {
    return Buffer.from([(major << 5) | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const out = Buffer.alloc(3);
    out[0] = (major << 5) | 25;
    out.writeUInt16BE(Number(value), 1);
    return out;
  }
  if (value <= 0xffffffffn) {
    const out = Buffer.alloc(5);
    out[0] = (major << 5) | 26;
    out.writeUInt32BE(Number(value), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = (major << 5) | 27;
  out.writeBigUInt64BE(value, 1);
  return out;
};

const encodeCborNodeWithDefiniteMaps = (node: CborNode): Buffer => {
  switch (node.kind) {
    case "uint":
      return encodeCborHeader(0, node.value);
    case "nint":
      return encodeCborHeader(1, node.value);
    case "bytes":
      return Buffer.concat([
        encodeCborHeader(2, BigInt(node.value.length)),
        node.value,
      ]);
    case "array": {
      const items = node.items.map(encodeCborNodeWithDefiniteMaps);
      if (node.indefinite) {
        return Buffer.concat([
          Buffer.from([0x9f]),
          ...items,
          Buffer.from([0xff]),
        ]);
      }
      return Buffer.concat([
        encodeCborHeader(4, BigInt(node.items.length)),
        ...items,
      ]);
    }
    case "map": {
      const entries = node.entries
        .map(([key, value]) => {
          const encodedKey = encodeCborNodeWithDefiniteMaps(key);
          const encodedValue = encodeCborNodeWithDefiniteMaps(value);
          return { encodedKey, encodedValue };
        })
        .sort((left, right) =>
          Buffer.compare(left.encodedKey, right.encodedKey),
        );
      return Buffer.concat([
        encodeCborHeader(5, BigInt(entries.length)),
        ...entries.flatMap(({ encodedKey, encodedValue }) => [
          encodedKey,
          encodedValue,
        ]),
      ]);
    }
    case "tag":
      return Buffer.concat([
        encodeCborHeader(6, node.tag),
        encodeCborNodeWithDefiniteMaps(node.value),
      ]);
  }
};

const aikenSerialisedPlutusDataCbor = (cbor: string): string => {
  const input = Buffer.from(cbor, "hex");
  const parsed = parseCborNode(input, 0);
  if (parsed.offset !== input.length) {
    throw new Error("Unexpected trailing bytes in PlutusData CBOR");
  }
  return encodeCborNodeWithDefiniteMaps(parsed.node).toString("hex");
};

const encodeHexBytesData = (hex: string): unknown =>
  Data.from(Data.to(hex as any, Data.Bytes()));

const formatCauseSummary = (cause: unknown): string => {
  if (cause instanceof Error && cause.message.length > 0) {
    return cause.message;
  }
  if (typeof cause === "string") {
    return cause;
  }
  return String(cause);
};

const fail = (
  message: string,
  cause: unknown,
): Effect.Effect<never, ReservePayoutTxError> =>
  Effect.fail(new ReservePayoutTxError({ message, cause }));

const requireNetwork = (
  lucid: LucidEvolution,
): Effect.Effect<Network, ReservePayoutTxError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* fail(
        "Cardano network not found while preparing reserve/payout transaction",
        "Lucid network configuration is undefined",
      );
    }
    return network;
  });

const networkId = (network: Network): number => (network === "Mainnet" ? 1 : 0);

const scriptRewardAddress = (network: Network, script: Script): string => {
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(networkId(network), credential)
    .to_address()
    .to_bech32();
};

const credentialFromAddressData = (credential: SDK.CredentialD): Credential => {
  if ("PublicKeyCredential" in credential) {
    return { type: "Key", hash: credential.PublicKeyCredential[0] };
  }
  return { type: "Script", hash: credential.ScriptCredential[0] };
};

const addressDataToBech32 = (
  network: Network,
  address: SDK.AddressData,
): string => {
  const paymentCredential = credentialFromAddressData(
    address.paymentCredential,
  );
  if (address.stakeCredential === null) {
    return credentialToAddress(network, paymentCredential);
  }
  if ("Inline" in address.stakeCredential) {
    return credentialToAddress(
      network,
      paymentCredential,
      credentialFromAddressData(address.stakeCredential.Inline[0]),
    );
  }
  throw new Error(
    "Pointer stake credentials are not supported by node builders",
  );
};

const cardanoDatumToOutputDatum = (
  datum: SDK.CardanoDatum,
): OutputDatum | undefined => {
  if (datum === "NoDatum") {
    return undefined;
  }
  if ("DatumHash" in datum) {
    return {
      kind: "hash",
      value: datum.DatumHash.hash,
    };
  }
  return {
    kind: "inline",
    value: Data.to(datum.InlineDatum.data as any, Data.Any() as any),
  };
};

const payToAddressWithCardanoDatum = (
  tx: TxBuilder,
  address: string,
  datum: SDK.CardanoDatum,
  assets: Assets,
): TxBuilder => {
  const outputDatum = cardanoDatumToOutputDatum(datum);
  return outputDatum === undefined
    ? tx.pay.ToAddress(address, assets)
    : tx.pay.ToAddressWithData(address, outputDatum, assets);
};

const assetsEqual = (left: Assets, right: Assets): boolean => {
  const normalizedLeft = normalizeAssets(left);
  const normalizedRight = normalizeAssets(right);
  const leftEntries = Object.entries(normalizedLeft).sort(([a], [b]) =>
    a.localeCompare(b),
  );
  const rightEntries = Object.entries(normalizedRight).sort(([a], [b]) =>
    a.localeCompare(b),
  );
  if (leftEntries.length !== rightEntries.length) {
    return false;
  }
  return leftEntries.every(
    ([unit, quantity], index) =>
      rightEntries[index]?.[0] === unit &&
      rightEntries[index]?.[1] === quantity,
  );
};

const normalizeAssets = (assets: Assets): Assets =>
  Object.fromEntries(
    Object.entries(assets).filter(([, quantity]) => quantity !== 0n),
  ) as Assets;

const addAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = { ...left };
  for (const [unit, quantity] of Object.entries(right)) {
    result[unit] = (result[unit] ?? 0n) + quantity;
  }
  return normalizeAssets(result as Assets);
};

const negateAssets = (assets: Assets): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, quantity]) => [unit, -quantity]),
  ) as Assets;

const subtractAssets = (left: Assets, right: Assets): Assets =>
  addAssets(left, negateAssets(right));

const removeAssetUnit = (
  assets: Assets,
  unit: string,
  expectedQuantity: bigint,
): Assets => {
  const actual = assets[unit] ?? 0n;
  if (actual !== expectedQuantity) {
    throw new Error(
      `Expected unit ${unit} quantity ${expectedQuantity.toString()}, got ${actual.toString()}`,
    );
  }
  return subtractAssets(assets, { [unit]: expectedQuantity });
};

export const assetsToValue = (assets: Assets): SDK.Value => {
  const outer = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(normalizeAssets(assets))) {
    if (quantity === 0n) {
      continue;
    }
    const policyId =
      unit === LOVELACE_UNIT ? ADA_POLICY_ID : unit.slice(0, 56).toLowerCase();
    const assetName =
      unit === LOVELACE_UNIT ? ADA_ASSET_NAME : unit.slice(56).toLowerCase();
    const inner = outer.get(policyId) ?? new Map<string, bigint>();
    inner.set(assetName, (inner.get(assetName) ?? 0n) + quantity);
    outer.set(policyId, inner);
  }
  return outer;
};

export const valueToAssets = (value: SDK.Value): Assets => {
  const assets: Record<string, bigint> = {};
  for (const [policyId, inner] of value.entries()) {
    for (const [assetName, quantity] of inner.entries()) {
      const unit =
        policyId === ADA_POLICY_ID && assetName === ADA_ASSET_NAME
          ? LOVELACE_UNIT
          : `${policyId}${assetName}`;
      assets[unit] = (assets[unit] ?? 0n) + quantity;
    }
  }
  return normalizeAssets(assets as Assets);
};

const minPositiveAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = {};
  for (const [unit, leftQuantity] of Object.entries(left)) {
    const rightQuantity = right[unit] ?? 0n;
    const taken =
      leftQuantity <= 0n || rightQuantity <= 0n
        ? 0n
        : leftQuantity < rightQuantity
          ? leftQuantity
          : rightQuantity;
    if (taken > 0n) {
      result[unit] = taken;
    }
  }
  return result as Assets;
};

const assertAssetsNonNegative = (assets: Assets, context: string): void => {
  const negative = Object.entries(assets).filter(
    ([, quantity]) => quantity < 0n,
  );
  if (negative.length > 0) {
    throw new Error(
      `${context} contains negative quantities: ${negative
        .map(([unit, quantity]) => `${unit}=${quantity.toString()}`)
        .join(",")}`,
    );
  }
};

const assertNoAssetExceeds = (
  actual: Assets,
  target: Assets,
  context: string,
): void => {
  for (const [unit, quantity] of Object.entries(actual)) {
    if (quantity > (target[unit] ?? 0n)) {
      throw new Error(
        `${context} exceeds target for ${unit}: actual=${quantity.toString()},target=${(
          target[unit] ?? 0n
        ).toString()}`,
      );
    }
  }
};

const isPureAdaUtxo = (utxo: UTxO): boolean =>
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === LOVELACE_UNIT || quantity === 0n,
  );

const isPlainPureAdaUtxo = (utxo: UTxO): boolean =>
  utxo.scriptRef === undefined && isPureAdaUtxo(utxo);

type ProviderLedgerEntry = {
  readonly utxo?: UTxO;
  readonly spent?: boolean;
};

type ProviderWithVisibleLedger = {
  readonly ledger?: Record<string, ProviderLedgerEntry | undefined>;
  readonly mempool?: Record<string, ProviderLedgerEntry | undefined>;
};

const isProviderSpendableUtxo = (
  lucid: LucidEvolution,
  utxo: UTxO,
): boolean => {
  const provider = lucid.config().provider as ProviderWithVisibleLedger;
  const outRefKey = `${utxo.txHash}${utxo.outputIndex.toString()}`;
  const hasVisibleProviderState =
    provider.ledger !== undefined || provider.mempool !== undefined;
  const entry = provider.ledger?.[outRefKey] ?? provider.mempool?.[outRefKey];
  if (entry === undefined) {
    return !hasVisibleProviderState;
  }
  return entry.spent !== true;
};

const fetchProviderVisibleWalletInputsProgram = (
  lucid: LucidEvolution,
): Effect.Effect<readonly UTxO[], SDK.LucidError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to fetch wallet address for reserve/payout transaction",
          cause,
        }),
    });
    const provider = lucid.config().provider as ProviderWithVisibleLedger;
    const visibleProviderEntries = [
      ...Object.values(provider.ledger ?? {}),
      ...Object.values(provider.mempool ?? {}),
    ];
    if (visibleProviderEntries.length > 0) {
      return visibleProviderEntries.flatMap((entry) => {
        if (
          entry === undefined ||
          entry.spent === true ||
          entry.utxo === undefined ||
          entry.utxo.address !== walletAddress
        ) {
          return [];
        }
        return [entry.utxo];
      });
    }
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(walletAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to fetch provider-visible wallet UTxOs for reserve/payout transaction",
          cause,
        }),
    });
    return walletUtxos.filter((utxo) => isProviderSpendableUtxo(lucid, utxo));
  });

const selectFeeInputProgram = (
  lucid: LucidEvolution,
  explicitFeeInput: UTxO | undefined,
  excluded: readonly OutRefLike[],
): Effect.Effect<UTxO, ReservePayoutTxError | SDK.LucidError> =>
  Effect.gen(function* () {
    const excludedKeys = new Set(excluded.map(outRefLabel));
    if (explicitFeeInput !== undefined) {
      if (excludedKeys.has(outRefLabel(explicitFeeInput))) {
        return yield* fail(
          "Explicit fee input overlaps a protected reserve/payout transaction input",
          {
            feeInput: outRefLabel(explicitFeeInput),
          },
        );
      }
      if (explicitFeeInput.scriptRef !== undefined) {
        return yield* fail(
          "Explicit fee input for reserve/payout transaction must not carry a reference script",
          {
            feeInput: outRefLabel(explicitFeeInput),
          },
        );
      }
      if (!isPlainPureAdaUtxo(explicitFeeInput)) {
        return yield* fail(
          "Explicit fee input for reserve/payout transaction must be pure ADA",
          {
            feeInput: outRefLabel(explicitFeeInput),
            assets: explicitFeeInput.assets,
          },
        );
      }
      if ((explicitFeeInput.assets.lovelace ?? 0n) <= 0n) {
        return yield* fail(
          "Explicit fee input for reserve/payout transaction has no ADA",
          {
            feeInput: outRefLabel(explicitFeeInput),
            assets: explicitFeeInput.assets,
          },
        );
      }
      return explicitFeeInput;
    }
    const walletUtxos = yield* fetchProviderVisibleWalletInputsProgram(lucid);
    const candidates = walletUtxos
      .filter((utxo) => !excludedKeys.has(outRefLabel(utxo)))
      .filter((utxo) => isPlainPureAdaUtxo(utxo))
      .filter((utxo) => (utxo.assets.lovelace ?? 0n) > 0n)
      .sort((left, right) => {
        const leftLovelace = left.assets.lovelace ?? 0n;
        const rightLovelace = right.assets.lovelace ?? 0n;
        if (leftLovelace === rightLovelace) {
          return compareOutRefs(left, right);
        }
        return leftLovelace > rightLovelace ? -1 : 1;
      });
    const selected = candidates[0];
    if (selected === undefined) {
      return yield* fail(
        "Failed to select fee input for reserve/payout transaction",
        "wallet has no pure-ADA UTxO outside the protocol input set",
      );
    }
    return selected;
  });

const fetchHubOracleReferenceProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  explicit: UTxO | undefined,
): Effect.Effect<
  UTxO,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
> =>
  Effect.gen(function* () {
    if (explicit !== undefined) {
      return explicit;
    }
    const network = yield* requireNetwork(lucid);
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    const hubOracleUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch hub oracle reference UTxO",
          cause,
        }),
    });
    const spendableHubOracleUtxos = hubOracleUtxos.filter((utxo) =>
      isProviderSpendableUtxo(lucid, utxo),
    );
    if (spendableHubOracleUtxos.length !== 1) {
      return yield* fail("Failed to fetch the hub oracle reference UTxO", {
        address: hubOracleAddress,
        unit: hubOracleUnit,
        found: spendableHubOracleUtxos.map(outRefLabel),
      });
    }
    const actual = spendableHubOracleUtxos[0]!;
    if ((actual.assets[hubOracleUnit] ?? 0n) !== 1n) {
      return yield* fail("Hub oracle reference UTxO is not authenticated", {
        hubOracleRefInput: outRefLabel(actual),
        unit: hubOracleUnit,
        quantity: (actual.assets[hubOracleUnit] ?? 0n).toString(),
      });
    }
    if (actual.datum === undefined) {
      return yield* fail("Hub oracle reference UTxO has no inline datum", {
        hubOracleRefInput: outRefLabel(actual),
      });
    }
    const expectedDatum = yield* SDK.makeHubOracleDatum(contracts);
    const actualDatum = yield* Effect.try({
      try: () => Data.from(actual.datum!, SDK.HubOracleDatum),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: "Failed to decode hub oracle reference datum",
          cause,
        }),
    });
    const actualDatumCbor = Data.to(actualDatum, SDK.HubOracleDatum);
    const expectedDatumCbor = Data.to(expectedDatum, SDK.HubOracleDatum);
    if (actualDatumCbor !== expectedDatumCbor) {
      return yield* fail(
        "On-chain hub oracle deployment does not match the locally configured contracts",
        {
          expectedDatumCbor,
          actualDatumCbor,
        },
      );
    }
    return actual;
  });

const resolveReferenceScriptsProgram = (
  lucid: LucidEvolution,
  address: string | undefined,
  targets: readonly {
    readonly name: string;
    readonly script: Script;
  }[],
  explicit?: ReservePayoutReferenceScripts,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (address === undefined) {
      return [];
    }
    const unresolvedTargets = targets.filter(
      (target) => !hasExplicitReferenceScript(explicit, target.name),
    );
    if (unresolvedTargets.length <= 0) {
      return [];
    }
    return yield* fetchReferenceScriptUtxosProgram(
      lucid,
      address,
      unresolvedTargets,
    );
  });

const hasExplicitReferenceScript = (
  explicit: ReservePayoutReferenceScripts | undefined,
  name: string,
): boolean => {
  if (explicit === undefined) {
    return false;
  }
  switch (name) {
    case "deposit minting":
      return explicit.depositMinting !== undefined;
    case "deposit spending":
      return explicit.depositSpending !== undefined;
    case "deposit witness certificate":
      return explicit.depositWitnessCertificate !== undefined;
    case "withdrawal minting":
      return explicit.withdrawalMinting !== undefined;
    case "withdrawal spending":
      return explicit.withdrawalSpending !== undefined;
    case "withdrawal witness certificate":
      return explicit.withdrawalWitnessCertificate !== undefined;
    case "membership proof withdrawal":
      return explicit.membershipProofWithdrawal !== undefined;
    case "reserve spending":
      return explicit.reserveSpending !== undefined;
    case "payout spending":
      return explicit.payoutSpending !== undefined;
    case "payout minting":
      return explicit.payoutMinting !== undefined;
    default:
      return false;
  }
};

const mergeReferenceScripts = (
  explicit: ReservePayoutReferenceScripts | undefined,
  resolved: readonly ReferenceScriptResolved[],
): ReservePayoutReferenceScripts => ({
  ...explicit,
  depositMinting:
    explicit?.depositMinting ??
    resolved.find((entry) => entry.name === "deposit minting")?.utxo,
  depositSpending:
    explicit?.depositSpending ??
    resolved.find((entry) => entry.name === "deposit spending")?.utxo,
  depositWitnessCertificate:
    explicit?.depositWitnessCertificate ??
    resolved.find((entry) => entry.name === "deposit witness certificate")
      ?.utxo,
  withdrawalMinting:
    explicit?.withdrawalMinting ??
    resolved.find((entry) => entry.name === "withdrawal minting")?.utxo,
  withdrawalSpending:
    explicit?.withdrawalSpending ??
    resolved.find((entry) => entry.name === "withdrawal spending")?.utxo,
  withdrawalWitnessCertificate:
    explicit?.withdrawalWitnessCertificate ??
    resolved.find((entry) => entry.name === "withdrawal witness certificate")
      ?.utxo,
  membershipProofWithdrawal:
    explicit?.membershipProofWithdrawal ??
    resolved.find((entry) => entry.name === "membership proof withdrawal")
      ?.utxo,
  reserveSpending:
    explicit?.reserveSpending ??
    resolved.find((entry) => entry.name === "reserve spending")?.utxo,
  payoutSpending:
    explicit?.payoutSpending ??
    resolved.find((entry) => entry.name === "payout spending")?.utxo,
  payoutMinting:
    explicit?.payoutMinting ??
    resolved.find((entry) => entry.name === "payout minting")?.utxo,
});

const referenceInputs = (
  hubOracleRefInput: UTxO,
  additional: readonly (UTxO | undefined)[],
): readonly UTxO[] =>
  dedupeByOutRef([
    hubOracleRefInput,
    ...additional.filter((utxo): utxo is UTxO => utxo !== undefined),
  ]);

const attachIfMissing = (
  tx: TxBuilder,
  script: Script,
  referenceScript: UTxO | undefined,
): TxBuilder => (referenceScript === undefined ? tx.attach.Script(script) : tx);

const completeWithTwoPassLayoutProgram = <L>({
  label,
  lucid,
  initialLayout,
  walletInputExclusions = [],
  makeTx,
  deriveLayout,
  sameLayout,
}: CompleteWithLayoutParams<L>): Effect.Effect<
  BuiltReservePayoutTx<L>,
  ReservePayoutTxError
> =>
  Effect.gen(function* () {
    const excludedWalletInputs = new Set(walletInputExclusions.map(outRefLabel));
    const walletInputs = yield* fetchProviderVisibleWalletInputsProgram(lucid).pipe(
      Effect.mapError(
        (cause) =>
          new ReservePayoutTxError({
            message: `Failed to fetch wallet inputs for ${label} transaction completion: ${formatCauseSummary(cause)}`,
            cause,
          }),
      ),
      Effect.map((utxos) =>
        utxos.filter((utxo) => !excludedWalletInputs.has(outRefLabel(utxo))),
      ),
    );
    const draft = yield* Effect.tryPromise({
      try: () =>
        withStubbedProviderEvaluation(lucid, () =>
          makeTx(initialLayout).complete({
            localUPLCEval: true,
            presetWalletInputs: [...walletInputs],
          }),
        ),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to build ${label} draft transaction: ${formatCauseSummary(cause)}`,
          cause,
        }),
    });
    const resolvedLayout = yield* Effect.try({
      try: () => deriveLayout(draft.toTransaction()),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to derive ${label} layout from balanced draft transaction`,
          cause,
        }),
    });

    const final = yield* Effect.tryPromise({
      try: () =>
        makeTx(resolvedLayout).complete({
          localUPLCEval: true,
          presetWalletInputs: [...walletInputs],
        }),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to build final ${label} transaction with real local UPLC evaluation: ${formatCauseSummary(cause)}`,
          cause,
        }),
    });

    const finalLayout = yield* Effect.try({
      try: () => deriveLayout(final.toTransaction()),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to derive ${label} layout from final transaction`,
          cause,
        }),
    });

    if (!sameLayout(resolvedLayout, finalLayout)) {
      return yield* fail(`${label} transaction layout was unstable`, {
        initialLayout,
        resolvedLayout,
        finalLayout,
      });
    }

    return {
      tx: final,
      layout: finalLayout,
    };
  });

const txInfoPurposeRank = (tag: number): number => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return 0;
    case CML.RedeemerTag.Mint:
      return 1;
    case CML.RedeemerTag.Cert:
      return 2;
    case CML.RedeemerTag.Reward:
      return 3;
    case CML.RedeemerTag.Voting:
      return 4;
    case CML.RedeemerTag.Proposing:
      return 5;
    default:
      return Number.MAX_SAFE_INTEGER;
  }
};

type RedeemerPointerLike = {
  readonly tag: number;
  readonly index: bigint;
};

const samePointer = (
  left: RedeemerPointerLike,
  right: RedeemerPointerLike,
): boolean => left.tag === right.tag && left.index === right.index;

const expectedTxInfoIndex = (
  pointers: readonly RedeemerPointerLike[],
  target: RedeemerPointerLike,
): bigint => {
  const contextIndex = pointers.findIndex((pointer) =>
    samePointer(pointer, target),
  );
  if (contextIndex < 0) {
    throw new Error(
      `Expected redeemer pointer missing: tag=${target.tag.toString()},index=${target.index.toString()}`,
    );
  }
  const ordered = pointers
    .map((pointer, index) => ({ pointer, index }))
    .sort((left, right) => {
      const rankLeft = txInfoPurposeRank(left.pointer.tag);
      const rankRight = txInfoPurposeRank(right.pointer.tag);
      if (rankLeft !== rankRight) {
        return rankLeft - rankRight;
      }
      if (left.pointer.index !== right.pointer.index) {
        return left.pointer.index < right.pointer.index ? -1 : 1;
      }
      return left.index - right.index;
    });
  const txInfoIndex = ordered.findIndex(
    (entry) => entry.index === contextIndex,
  );
  if (txInfoIndex < 0) {
    throw new Error("Failed to derive expected tx-info redeemer index");
  }
  return BigInt(txInfoIndex);
};

const actualTxInfoIndex = (
  tx: CML.Transaction,
  target: RedeemerPointerLike,
): bigint => {
  const pointers = getRedeemerPointersInContextOrder(tx);
  const contextIndex = pointers.findIndex((pointer) =>
    samePointer(pointer, target),
  );
  if (contextIndex < 0) {
    throw new Error(
      `Transaction missing redeemer pointer tag=${target.tag.toString()},index=${target.index.toString()}`,
    );
  }
  const txInfoIndexes = getTxInfoRedeemerIndexes(pointers);
  const txInfoIndex = txInfoIndexes[contextIndex];
  if (txInfoIndex === undefined || txInfoIndex < 0) {
    throw new Error(
      `Transaction missing tx-info index for redeemer pointer tag=${target.tag.toString()},index=${target.index.toString()}`,
    );
  }
  return BigInt(txInfoIndex);
};

const comparePolicyIds = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

const mintPointerIndex = (
  policyIds: readonly string[],
  targetPolicyId: string,
): bigint => {
  const sorted = [
    ...new Set(policyIds.map((policy) => policy.toLowerCase())),
  ].sort(comparePolicyIds);
  const index = sorted.indexOf(targetPolicyId.toLowerCase());
  if (index < 0) {
    throw new Error(
      `Mint policy ${targetPolicyId} missing from mint policy set`,
    );
  }
  return BigInt(index);
};

const requireReferenceInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
): bigint => {
  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    throw new Error("Transaction did not include reference inputs");
  }
  const index = findOutRefIndex(
    collectSortedInputOutRefs(referenceInputs),
    target,
  );
  if (index === undefined) {
    throw new Error(
      `Reference input ${outRefLabel(target)} missing from transaction`,
    );
  }
  return BigInt(index);
};

const requireInputIndex = (tx: CML.Transaction, target: UTxO): bigint => {
  const index = findOutRefIndex(
    collectSortedInputOutRefs(tx.body().inputs()),
    target,
  );
  if (index === undefined) {
    throw new Error(`Input ${outRefLabel(target)} missing from transaction`);
  }
  return BigInt(index);
};

const requireOutput = (
  tx: CML.Transaction,
  predicate: (output: IndexedTxOutput) => boolean,
  description: string,
): IndexedTxOutput => {
  const matches = collectIndexedOutputs(tx.body().outputs()).filter(predicate);
  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one ${description} output, found ${matches.length.toString()}`,
    );
  }
  return matches[0]!;
};

const outputHasNoDatum = (output: IndexedTxOutput): boolean =>
  output.datum === undefined && output.datumHash === undefined;

const outputDatumMatches = (
  output: IndexedTxOutput,
  datum: SDK.CardanoDatum,
): boolean => {
  if (datum === "NoDatum") {
    return outputHasNoDatum(output);
  }
  if ("DatumHash" in datum) {
    return (
      output.datumHash === datum.DatumHash.hash && output.datum === undefined
    );
  }
  return (
    output.datum === Data.to(datum.InlineDatum.data as any, Data.Any() as any)
  );
};

const settlementDatumFromInput = (
  settlementRefInput: UTxO,
): SDK.SettlementDatum => {
  if (settlementRefInput.datum == null) {
    throw new Error(
      `Settlement reference input ${outRefLabel(settlementRefInput)} has no inline datum`,
    );
  }
  return Data.from(
    settlementRefInput.datum,
    SDK.SettlementDatum,
  ) as SDK.SettlementDatum;
};

const encodeMembershipProofWithdrawalRedeemer = (
  root: string,
  keyCbor: string,
  valueCbor: string,
  proof: SDK.Proof,
): string => {
  const rootData = Data.from(Data.to(root, SDK.MerkleRoot));
  const keyData = encodeHexBytesData(aikenSerialisedPlutusDataCbor(keyCbor));
  const valueData = encodeHexBytesData(
    aikenSerialisedPlutusDataCbor(valueCbor),
  );
  const proofData = Data.from(Data.to(proof, SDK.Proof));
  return Data.to(
    [rootData, keyData, valueData, proofData] as any,
    Data.Array(Data.Any()) as any,
  );
};

const applyEventWitnessUnregistration = (
  tx: TxBuilder,
  network: Network,
  witnessScript: Script,
  redeemer: string,
  referenceScript: UTxO | undefined,
): TxBuilder =>
  attachIfMissing(
    tx.deregister.Stake(scriptRewardAddress(network, witnessScript), redeemer),
    witnessScript,
    referenceScript,
  );

const applyMembershipProofWithdrawal = (
  tx: TxBuilder,
  network: Network,
  witness: MembershipProofWithdrawalWitness,
  redeemer: string,
  referenceScript: UTxO | undefined,
): TxBuilder =>
  (referenceScript === undefined ? tx.attach.Script(witness.script) : tx)
    .withdraw(
      scriptRewardAddress(network, witness.script),
      witness.amount ?? 0n,
      redeemer,
    );

type AbsorbDepositLayout = {
  readonly depositInputIndex: bigint;
  readonly reserveOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly witnessUnregistrationRedeemerIndex: bigint;
  readonly inclusionProofWithdrawalRedeemerIndex: bigint;
};

type InitializePayoutLayout = {
  readonly withdrawalInputIndex: bigint;
  readonly payoutOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly withdrawalBurnRedeemerIndex: bigint;
  readonly payoutMintRedeemerIndex: bigint;
  readonly withdrawalSpendRedeemerIndex: bigint;
  readonly witnessUnregistrationRedeemerIndex: bigint;
  readonly inclusionProofWithdrawalRedeemerIndex: bigint;
};

type AddReserveFundsLayout = {
  readonly payoutInputIndex: bigint;
  readonly reserveInputIndex: bigint;
  readonly payoutOutputIndex: bigint;
  readonly reserveChangeOutputIndex: bigint | null;
  readonly payoutSpendRedeemerIndex: bigint;
  readonly reserveSpendRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

type ConcludePayoutLayout = {
  readonly payoutInputIndex: bigint;
  readonly l1OutputIndex: bigint;
  readonly payoutSpendRedeemerIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

type RefundWithdrawalLayout = {
  readonly withdrawalInputIndex: bigint;
  readonly refundOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly witnessUnregistrationRedeemerIndex: bigint;
  readonly inclusionProofWithdrawalRedeemerIndex: bigint;
};

const sameAbsorbDepositLayout = (
  left: AbsorbDepositLayout,
  right: AbsorbDepositLayout,
): boolean =>
  left.depositInputIndex === right.depositInputIndex &&
  left.reserveOutputIndex === right.reserveOutputIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex &&
  left.settlementRefInputIndex === right.settlementRefInputIndex &&
  left.burnRedeemerIndex === right.burnRedeemerIndex &&
  left.witnessUnregistrationRedeemerIndex ===
    right.witnessUnregistrationRedeemerIndex &&
  left.inclusionProofWithdrawalRedeemerIndex ===
    right.inclusionProofWithdrawalRedeemerIndex;

const sameInitializePayoutLayout = (
  left: InitializePayoutLayout,
  right: InitializePayoutLayout,
): boolean =>
  left.withdrawalInputIndex === right.withdrawalInputIndex &&
  left.payoutOutputIndex === right.payoutOutputIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex &&
  left.settlementRefInputIndex === right.settlementRefInputIndex &&
  left.withdrawalBurnRedeemerIndex === right.withdrawalBurnRedeemerIndex &&
  left.payoutMintRedeemerIndex === right.payoutMintRedeemerIndex &&
  left.withdrawalSpendRedeemerIndex === right.withdrawalSpendRedeemerIndex &&
  left.witnessUnregistrationRedeemerIndex ===
    right.witnessUnregistrationRedeemerIndex &&
  left.inclusionProofWithdrawalRedeemerIndex ===
    right.inclusionProofWithdrawalRedeemerIndex;

const sameAddReserveFundsLayout = (
  left: AddReserveFundsLayout,
  right: AddReserveFundsLayout,
): boolean =>
  left.payoutInputIndex === right.payoutInputIndex &&
  left.reserveInputIndex === right.reserveInputIndex &&
  left.payoutOutputIndex === right.payoutOutputIndex &&
  left.reserveChangeOutputIndex === right.reserveChangeOutputIndex &&
  left.payoutSpendRedeemerIndex === right.payoutSpendRedeemerIndex &&
  left.reserveSpendRedeemerIndex === right.reserveSpendRedeemerIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex;

const sameConcludePayoutLayout = (
  left: ConcludePayoutLayout,
  right: ConcludePayoutLayout,
): boolean =>
  left.payoutInputIndex === right.payoutInputIndex &&
  left.l1OutputIndex === right.l1OutputIndex &&
  left.payoutSpendRedeemerIndex === right.payoutSpendRedeemerIndex &&
  left.burnRedeemerIndex === right.burnRedeemerIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex;

const sameRefundWithdrawalLayout = (
  left: RefundWithdrawalLayout,
  right: RefundWithdrawalLayout,
): boolean =>
  left.withdrawalInputIndex === right.withdrawalInputIndex &&
  left.refundOutputIndex === right.refundOutputIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex &&
  left.settlementRefInputIndex === right.settlementRefInputIndex &&
  left.burnRedeemerIndex === right.burnRedeemerIndex &&
  left.witnessUnregistrationRedeemerIndex ===
    right.witnessUnregistrationRedeemerIndex &&
  left.inclusionProofWithdrawalRedeemerIndex ===
    right.inclusionProofWithdrawalRedeemerIndex;

const initialAbsorbDepositLayout = ({
  inputs,
  referenceInputs,
  deposit,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly deposit: SDK.DepositUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): AbsorbDepositLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const pointers: RedeemerPointerLike[] = [
    {
      tag: CML.RedeemerTag.Spend,
      index: BigInt(findOutRefIndex(orderedInputs, deposit.utxo) ?? -1),
    },
    { tag: CML.RedeemerTag.Mint, index: 0n },
    { tag: CML.RedeemerTag.Cert, index: 0n },
    { tag: CML.RedeemerTag.Reward, index: 0n },
  ];
  return {
    depositInputIndex: BigInt(
      findOutRefIndex(orderedInputs, deposit.utxo) ?? -1,
    ),
    reserveOutputIndex: 0n,
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
    settlementRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, settlementRefInput) ?? -1,
    ),
    burnRedeemerIndex: expectedTxInfoIndex(pointers, pointers[1]!),
    witnessUnregistrationRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[2]!,
    ),
    inclusionProofWithdrawalRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[3]!,
    ),
  };
};

const deriveAbsorbDepositLayout = ({
  tx,
  deposit,
  depositUnit,
  reserveAddress,
  reserveAssets,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly deposit: SDK.DepositUTxO;
  readonly depositUnit: string;
  readonly reserveAddress: string;
  readonly reserveAssets: Assets;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): AbsorbDepositLayout => {
  const depositInputIndex = requireInputIndex(tx, deposit.utxo);
  const reserveOutput = requireOutput(
    tx,
    (output) =>
      output.address === reserveAddress &&
      outputHasNoDatum(output) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, reserveAssets),
    `reserve absorption output at ${reserveAddress}`,
  );
  const mintPointer = { tag: CML.RedeemerTag.Mint, index: 0n };
  const certPointer = { tag: CML.RedeemerTag.Cert, index: 0n };
  const rewardPointer = { tag: CML.RedeemerTag.Reward, index: 0n };
  if ((deposit.utxo.assets[depositUnit] ?? 0n) !== 1n) {
    throw new Error(
      `Deposit input does not contain exactly one ${depositUnit}`,
    );
  }
  return {
    depositInputIndex,
    reserveOutputIndex: BigInt(reserveOutput.index),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
    settlementRefInputIndex: requireReferenceInputIndex(tx, settlementRefInput),
    burnRedeemerIndex: actualTxInfoIndex(tx, mintPointer),
    witnessUnregistrationRedeemerIndex: actualTxInfoIndex(tx, certPointer),
    inclusionProofWithdrawalRedeemerIndex: actualTxInfoIndex(tx, rewardPointer),
  };
};

const initialInitializePayoutLayout = ({
  inputs,
  referenceInputs,
  withdrawal,
  hubOracleRefInput,
  settlementRefInput,
  withdrawalPolicyId,
  payoutPolicyId,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
  readonly withdrawalPolicyId: string;
  readonly payoutPolicyId: string;
}): InitializePayoutLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const withdrawalInputIndex = BigInt(
    findOutRefIndex(orderedInputs, withdrawal.utxo) ?? -1,
  );
  const withdrawalMintPointerIndex = mintPointerIndex(
    [withdrawalPolicyId, payoutPolicyId],
    withdrawalPolicyId,
  );
  const payoutMintPointerIndex = mintPointerIndex(
    [withdrawalPolicyId, payoutPolicyId],
    payoutPolicyId,
  );
  const pointers: RedeemerPointerLike[] = [
    { tag: CML.RedeemerTag.Spend, index: withdrawalInputIndex },
    { tag: CML.RedeemerTag.Mint, index: withdrawalMintPointerIndex },
    { tag: CML.RedeemerTag.Mint, index: payoutMintPointerIndex },
    { tag: CML.RedeemerTag.Cert, index: 0n },
    { tag: CML.RedeemerTag.Reward, index: 0n },
  ];
  return {
    withdrawalInputIndex,
    payoutOutputIndex: 0n,
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
    settlementRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, settlementRefInput) ?? -1,
    ),
    withdrawalBurnRedeemerIndex: expectedTxInfoIndex(pointers, pointers[1]!),
    payoutMintRedeemerIndex: expectedTxInfoIndex(pointers, pointers[2]!),
    withdrawalSpendRedeemerIndex: expectedTxInfoIndex(pointers, pointers[0]!),
    witnessUnregistrationRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[3]!,
    ),
    inclusionProofWithdrawalRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[4]!,
    ),
  };
};

const deriveInitializePayoutLayout = ({
  tx,
  withdrawal,
  payoutAddress,
  payoutAssets,
  payoutDatumCbor,
  hubOracleRefInput,
  settlementRefInput,
  withdrawalPolicyId,
  payoutPolicyId,
}: {
  readonly tx: CML.Transaction;
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly payoutAddress: string;
  readonly payoutAssets: Assets;
  readonly payoutDatumCbor: string;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
  readonly withdrawalPolicyId: string;
  readonly payoutPolicyId: string;
}): InitializePayoutLayout => {
  const withdrawalInputIndex = requireInputIndex(tx, withdrawal.utxo);
  const payoutOutput = requireOutput(
    tx,
    (output) =>
      output.address === payoutAddress &&
      output.datum === payoutDatumCbor &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, payoutAssets),
    `payout initialization output at ${payoutAddress}`,
  );
  const withdrawalMintPointer = {
    tag: CML.RedeemerTag.Mint,
    index: mintPointerIndex(
      [withdrawalPolicyId, payoutPolicyId],
      withdrawalPolicyId,
    ),
  };
  const payoutMintPointer = {
    tag: CML.RedeemerTag.Mint,
    index: mintPointerIndex(
      [withdrawalPolicyId, payoutPolicyId],
      payoutPolicyId,
    ),
  };
  return {
    withdrawalInputIndex,
    payoutOutputIndex: BigInt(payoutOutput.index),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
    settlementRefInputIndex: requireReferenceInputIndex(tx, settlementRefInput),
    withdrawalBurnRedeemerIndex: actualTxInfoIndex(tx, withdrawalMintPointer),
    payoutMintRedeemerIndex: actualTxInfoIndex(tx, payoutMintPointer),
    withdrawalSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: withdrawalInputIndex,
    }),
    witnessUnregistrationRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
    }),
    inclusionProofWithdrawalRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Reward,
      index: 0n,
    }),
  };
};

const initialAddReserveFundsLayout = ({
  inputs,
  referenceInputs,
  payoutInput,
  reserveInput,
  hubOracleRefInput,
  reserveChangeAssets,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly reserveChangeAssets: Assets;
}): AddReserveFundsLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const payoutInputIndex = BigInt(
    findOutRefIndex(orderedInputs, payoutInput) ?? -1,
  );
  const reserveInputIndex = BigInt(
    findOutRefIndex(orderedInputs, reserveInput) ?? -1,
  );
  const payoutPointer = { tag: CML.RedeemerTag.Spend, index: payoutInputIndex };
  const reservePointer = {
    tag: CML.RedeemerTag.Spend,
    index: reserveInputIndex,
  };
  const pointers = [payoutPointer, reservePointer];
  return {
    payoutInputIndex,
    reserveInputIndex,
    payoutOutputIndex: 0n,
    reserveChangeOutputIndex:
      Object.keys(normalizeAssets(reserveChangeAssets)).length === 0
        ? null
        : 1n,
    payoutSpendRedeemerIndex: expectedTxInfoIndex(pointers, payoutPointer),
    reserveSpendRedeemerIndex: expectedTxInfoIndex(pointers, reservePointer),
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
  };
};

const deriveAddReserveFundsLayout = ({
  tx,
  payoutInput,
  reserveInput,
  payoutAddress,
  payoutOutputAssets,
  payoutDatumCbor,
  reserveAddress,
  reserveChangeAssets,
  hubOracleRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
  readonly payoutAddress: string;
  readonly payoutOutputAssets: Assets;
  readonly payoutDatumCbor: string;
  readonly reserveAddress: string;
  readonly reserveChangeAssets: Assets;
  readonly hubOracleRefInput: UTxO;
}): AddReserveFundsLayout => {
  const payoutInputIndex = requireInputIndex(tx, payoutInput);
  const reserveInputIndex = requireInputIndex(tx, reserveInput);
  const payoutOutput = requireOutput(
    tx,
    (output) =>
      output.address === payoutAddress &&
      output.datum === payoutDatumCbor &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, payoutOutputAssets),
    `updated payout output at ${payoutAddress}`,
  );
  const normalizedReserveChange = normalizeAssets(reserveChangeAssets);
  const reserveChangeOutput =
    Object.keys(normalizedReserveChange).length === 0
      ? undefined
      : requireOutput(
          tx,
          (output) =>
            output.address === reserveAddress &&
            outputHasNoDatum(output) &&
            output.scriptRef === undefined &&
            assetsEqual(output.assets, normalizedReserveChange),
          `reserve change output at ${reserveAddress}`,
        );
  return {
    payoutInputIndex,
    reserveInputIndex,
    payoutOutputIndex: BigInt(payoutOutput.index),
    reserveChangeOutputIndex:
      reserveChangeOutput === undefined
        ? null
        : BigInt(reserveChangeOutput.index),
    payoutSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: payoutInputIndex,
    }),
    reserveSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: reserveInputIndex,
    }),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
  };
};

const initialConcludePayoutLayout = ({
  inputs,
  referenceInputs,
  payoutInput,
  hubOracleRefInput,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly payoutInput: UTxO;
  readonly hubOracleRefInput: UTxO;
}): ConcludePayoutLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const payoutInputIndex = BigInt(
    findOutRefIndex(orderedInputs, payoutInput) ?? -1,
  );
  const spendPointer = { tag: CML.RedeemerTag.Spend, index: payoutInputIndex };
  const burnPointer = { tag: CML.RedeemerTag.Mint, index: 0n };
  const pointers = [spendPointer, burnPointer];
  return {
    payoutInputIndex,
    l1OutputIndex: 0n,
    payoutSpendRedeemerIndex: expectedTxInfoIndex(pointers, spendPointer),
    burnRedeemerIndex: expectedTxInfoIndex(pointers, burnPointer),
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
  };
};

const deriveConcludePayoutLayout = ({
  tx,
  payoutInput,
  l1Address,
  l1Datum,
  l1Assets,
  hubOracleRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly payoutInput: UTxO;
  readonly l1Address: string;
  readonly l1Datum: SDK.CardanoDatum;
  readonly l1Assets: Assets;
  readonly hubOracleRefInput: UTxO;
}): ConcludePayoutLayout => {
  const payoutInputIndex = requireInputIndex(tx, payoutInput);
  const l1Output = requireOutput(
    tx,
    (output) =>
      output.address === l1Address &&
      outputDatumMatches(output, l1Datum) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, l1Assets),
    `payout destination output at ${l1Address}`,
  );
  return {
    payoutInputIndex,
    l1OutputIndex: BigInt(l1Output.index),
    payoutSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: payoutInputIndex,
    }),
    burnRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
    }),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
  };
};

const initialRefundWithdrawalLayout = ({
  inputs,
  referenceInputs,
  withdrawal,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): RefundWithdrawalLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const withdrawalInputIndex = BigInt(
    findOutRefIndex(orderedInputs, withdrawal.utxo) ?? -1,
  );
  const pointers: RedeemerPointerLike[] = [
    { tag: CML.RedeemerTag.Spend, index: withdrawalInputIndex },
    { tag: CML.RedeemerTag.Mint, index: 0n },
    { tag: CML.RedeemerTag.Cert, index: 0n },
    { tag: CML.RedeemerTag.Reward, index: 0n },
  ];
  return {
    withdrawalInputIndex,
    refundOutputIndex: 0n,
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
    settlementRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, settlementRefInput) ?? -1,
    ),
    burnRedeemerIndex: expectedTxInfoIndex(pointers, pointers[1]!),
    witnessUnregistrationRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[2]!,
    ),
    inclusionProofWithdrawalRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[3]!,
    ),
  };
};

const deriveRefundWithdrawalLayout = ({
  tx,
  withdrawal,
  refundAddress,
  refundDatum,
  refundAssets,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly refundAddress: string;
  readonly refundDatum: SDK.CardanoDatum;
  readonly refundAssets: Assets;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): RefundWithdrawalLayout => {
  const withdrawalInputIndex = requireInputIndex(tx, withdrawal.utxo);
  const refundOutput = requireOutput(
    tx,
    (output) =>
      output.address === refundAddress &&
      outputDatumMatches(output, refundDatum) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, refundAssets),
    `withdrawal refund output at ${refundAddress}`,
  );
  return {
    withdrawalInputIndex,
    refundOutputIndex: BigInt(refundOutput.index),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
    settlementRefInputIndex: requireReferenceInputIndex(tx, settlementRefInput),
    burnRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
    }),
    witnessUnregistrationRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
    }),
    inclusionProofWithdrawalRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Reward,
      index: 0n,
    }),
  };
};

export const buildAbsorbConfirmedDepositToReserveTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AbsorbConfirmedDepositConfig,
): Effect.Effect<
  BuiltReservePayoutTx<AbsorbDepositLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
      config.deposit.assetName,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "deposit minting", script: contracts.deposit.mintingScript },
        { name: "deposit spending", script: contracts.deposit.spendingScript },
        { name: "deposit witness certificate", script: witnessScript },
        {
          name: "membership proof withdrawal",
          script: config.membershipProofWithdrawal.script,
        },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const depositUnit = toUnit(
      contracts.deposit.policyId,
      config.deposit.assetName,
    );
    const reserveAssets = removeAssetUnit(
      config.deposit.utxo.assets,
      depositUnit,
      1n,
    );
    const settlementDatum = settlementDatumFromInput(config.settlementRefInput);
    const membershipRedeemer = encodeMembershipProofWithdrawalRedeemer(
      settlementDatum.deposits_root,
      Data.to(config.deposit.datum.event.id, SDK.OutputReference),
      Data.to(config.deposit.datum.event.info, SDK.DepositInfo),
      config.membershipProof,
    );
    const witnessRedeemer = SDK.encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.deposit.policyId,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.deposit.utxo,
      config.settlementRefInput,
      hubOracleRefInput,
      ...(refs.depositMinting === undefined ? [] : [refs.depositMinting]),
      ...(refs.depositSpending === undefined ? [] : [refs.depositSpending]),
      ...(refs.depositWitnessCertificate === undefined
        ? []
        : [refs.depositWitnessCertificate]),
      ...(refs.membershipProofWithdrawal === undefined
        ? []
        : [refs.membershipProofWithdrawal]),
    ]);
    const txInputs = [config.deposit.utxo, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      config.settlementRefInput,
      refs.depositMinting,
      refs.depositSpending,
      refs.depositWitnessCertificate,
      refs.membershipProofWithdrawal,
    ]);
    const initialLayout = initialAbsorbDepositLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      deposit: config.deposit,
      hubOracleRefInput,
      settlementRefInput: config.settlementRefInput,
    });
    if (
      initialLayout.depositInputIndex < 0n ||
      initialLayout.hubRefInputIndex < 0n
    ) {
      return yield* fail("Failed to derive initial deposit absorption layout", {
        initialLayout,
      });
    }
    const makeTx = (layout: AbsorbDepositLayout): TxBuilder => {
      const depositSpendRedeemer: SDK.DepositSpendRedeemer = {
        input_index: layout.depositInputIndex,
        output_index: layout.reserveOutputIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
        settlement_ref_input_index: layout.settlementRefInputIndex,
        mint_redeemer_index: layout.burnRedeemerIndex,
        membership_proof: config.membershipProof,
        inclusion_proof_script_withdraw_redeemer_index:
          layout.inclusionProofWithdrawalRedeemerIndex,
      };
      const depositBurnRedeemer: SDK.DepositMintRedeemer = {
        BurnEventNFT: {
          nonce_asset_name: config.deposit.assetName,
          witness_unregistration_redeemer_index:
            layout.witnessUnregistrationRedeemerIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.deposit.utxo],
          Data.to(depositSpendRedeemer, SDK.DepositSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [depositUnit]: -1n },
          Data.to(depositBurnRedeemer, SDK.DepositMintRedeemer),
        )
        .pay.ToAddress(contracts.reserve.spendingScriptAddress, reserveAssets);
      tx = applyEventWitnessUnregistration(
        tx,
        network,
        witnessScript,
        witnessRedeemer,
        refs.depositWitnessCertificate,
      );
      tx = applyMembershipProofWithdrawal(
        tx,
        network,
        config.membershipProofWithdrawal,
        membershipRedeemer,
        refs.membershipProofWithdrawal,
      );
      tx = attachIfMissing(
        tx,
        contracts.deposit.spendingScript,
        refs.depositSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.deposit.mintingScript,
        refs.depositMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "deposit absorption",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveAbsorbDepositLayout({
          tx,
          deposit: config.deposit,
          depositUnit,
          reserveAddress: contracts.reserve.spendingScriptAddress,
          reserveAssets,
          hubOracleRefInput,
          settlementRefInput: config.settlementRefInput,
        }),
      sameLayout: sameAbsorbDepositLayout,
    }).pipe(
      Effect.mapError((cause) =>
        cause instanceof ReservePayoutTxError
          ? cause
          : new ReservePayoutTxError({
              message: "Failed to build deposit absorption transaction",
              cause,
            }),
      ),
    );
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(
        `Reserve deposit absorption layout: ${formatLayout(built.layout)}`,
      ),
    ),
  );

export const buildInitializePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: InitializePayoutConfig,
): Effect.Effect<
  BuiltReservePayoutTx<InitializePayoutLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
      config.withdrawal.assetName,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        {
          name: "withdrawal minting",
          script: contracts.withdrawal.mintingScript,
        },
        {
          name: "withdrawal spending",
          script: contracts.withdrawal.spendingScript,
        },
        { name: "payout minting", script: contracts.payout.mintingScript },
        { name: "withdrawal witness certificate", script: witnessScript },
        {
          name: "membership proof withdrawal",
          script: config.membershipProofWithdrawal.script,
        },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const withdrawalUnit = toUnit(
      contracts.withdrawal.policyId,
      config.withdrawal.assetName,
    );
    const payoutUnit = toUnit(
      contracts.payout.policyId,
      config.withdrawal.assetName,
    );
    const payoutAssets = addAssets(
      removeAssetUnit(config.withdrawal.utxo.assets, withdrawalUnit, 1n),
      { [payoutUnit]: 1n },
    );
    const payoutDatum: SDK.PayoutDatum = {
      l2_value: config.withdrawal.datum.event.info.body.l2_value,
      l1_address: config.withdrawal.datum.event.info.body.l1_address,
      l1_datum: config.withdrawal.datum.event.info.body.l1_datum,
    };
    const payoutDatumCbor = Data.to(payoutDatum, SDK.PayoutDatum);
    const initialAccumulatorAssets = removeAssetUnit(
      payoutAssets,
      payoutUnit,
      1n,
    );
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    assertNoAssetExceeds(
      initialAccumulatorAssets,
      targetAssets,
      "Initial payout accumulator",
    );
    const settlementDatum = settlementDatumFromInput(config.settlementRefInput);
    const membershipRedeemer = encodeMembershipProofWithdrawalRedeemer(
      settlementDatum.withdrawals_root,
      Data.to(config.withdrawal.datum.event.id, SDK.OutputReference),
      Data.to(config.withdrawal.datum.event.info, SDK.WithdrawalInfo),
      config.membershipProof,
    );
    const witnessRedeemer = SDK.encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.withdrawal.policyId,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.withdrawal.utxo,
      config.settlementRefInput,
      hubOracleRefInput,
      ...(refs.payoutMinting === undefined ? [] : [refs.payoutMinting]),
      ...(refs.withdrawalMinting === undefined ? [] : [refs.withdrawalMinting]),
      ...(refs.withdrawalSpending === undefined
        ? []
        : [refs.withdrawalSpending]),
      ...(refs.withdrawalWitnessCertificate === undefined
        ? []
        : [refs.withdrawalWitnessCertificate]),
      ...(refs.membershipProofWithdrawal === undefined
        ? []
        : [refs.membershipProofWithdrawal]),
    ]);
    const txInputs = [config.withdrawal.utxo, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      config.settlementRefInput,
      refs.payoutMinting,
      refs.withdrawalMinting,
      refs.withdrawalSpending,
      refs.withdrawalWitnessCertificate,
      refs.membershipProofWithdrawal,
    ]);
    const initialLayout = initialInitializePayoutLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      withdrawal: config.withdrawal,
      hubOracleRefInput,
      settlementRefInput: config.settlementRefInput,
      withdrawalPolicyId: contracts.withdrawal.policyId,
      payoutPolicyId: contracts.payout.policyId,
    });
    const makeTx = (layout: InitializePayoutLayout): TxBuilder => {
      const withdrawalSpendRedeemer: SDK.WithdrawalSpendRedeemer = {
        input_index: layout.withdrawalInputIndex,
        output_index: layout.payoutOutputIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
        settlement_ref_input_index: layout.settlementRefInputIndex,
        burn_redeemer_index: layout.withdrawalBurnRedeemerIndex,
        payout_mint_redeemer_index: layout.payoutMintRedeemerIndex,
        membership_proof: config.membershipProof,
        inclusion_proof_script_withdraw_redeemer_index:
          layout.inclusionProofWithdrawalRedeemerIndex,
        purpose: "InitializePayout",
      };
      const withdrawalBurnRedeemer: SDK.WithdrawalMintRedeemer = {
        BurnEventNFT: {
          nonce_asset_name: config.withdrawal.assetName,
          witness_unregistration_redeemer_index:
            layout.witnessUnregistrationRedeemerIndex,
        },
      };
      const payoutMintRedeemer: SDK.PayoutMintRedeemer = {
        MintPayout: {
          withdrawal_utxo_out_ref: {
            transactionId: config.withdrawal.utxo.txHash,
            outputIndex: BigInt(config.withdrawal.utxo.outputIndex),
          },
          withdrawal_input_index: layout.withdrawalInputIndex,
          withdrawal_spend_redeemer_index: layout.withdrawalSpendRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.withdrawal.utxo],
          Data.to(withdrawalSpendRedeemer, SDK.WithdrawalSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [withdrawalUnit]: -1n },
          Data.to(withdrawalBurnRedeemer, SDK.WithdrawalMintRedeemer),
        )
        .mintAssets(
          { [payoutUnit]: 1n },
          Data.to(payoutMintRedeemer, SDK.PayoutMintRedeemer),
        )
        .pay.ToAddressWithData(
          contracts.payout.spendingScriptAddress,
          { kind: "inline", value: payoutDatumCbor },
          payoutAssets,
        );
      tx = applyEventWitnessUnregistration(
        tx,
        network,
        witnessScript,
        witnessRedeemer,
        refs.withdrawalWitnessCertificate,
      );
      tx = applyMembershipProofWithdrawal(
        tx,
        network,
        config.membershipProofWithdrawal,
        membershipRedeemer,
        refs.membershipProofWithdrawal,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.spendingScript,
        refs.withdrawalSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.mintingScript,
        refs.withdrawalMinting,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.mintingScript,
        refs.payoutMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "payout initialization",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveInitializePayoutLayout({
          tx,
          withdrawal: config.withdrawal,
          payoutAddress: contracts.payout.spendingScriptAddress,
          payoutAssets,
          payoutDatumCbor,
          hubOracleRefInput,
          settlementRefInput: config.settlementRefInput,
          withdrawalPolicyId: contracts.withdrawal.policyId,
          payoutPolicyId: contracts.payout.policyId,
        }),
      sameLayout: sameInitializePayoutLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(
        `Payout initialization layout: ${formatLayout(built.layout)}`,
      ),
    ),
  );

const decodePayoutDatum = (payoutInput: UTxO): SDK.PayoutDatum => {
  if (payoutInput.datum == null) {
    throw new Error(
      `Payout input ${outRefLabel(payoutInput)} has no inline datum`,
    );
  }
  return Data.from(payoutInput.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

const payoutAssetNameFromInput = (
  payoutInput: UTxO,
  payoutPolicyId: string,
): string => {
  const matches = Object.entries(payoutInput.assets).filter(
    ([unit, quantity]) =>
      unit.startsWith(payoutPolicyId) && unit.length >= 56 && quantity === 1n,
  );
  if (matches.length !== 1) {
    throw new Error(
      `Expected payout input ${outRefLabel(
        payoutInput,
      )} to contain exactly one payout NFT for policy ${payoutPolicyId}, found ${matches.length.toString()}`,
    );
  }
  return matches[0]![0].slice(56);
};

export const buildAddReserveFundsToPayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AddReserveFundsConfig,
): Effect.Effect<
  BuiltReservePayoutTx<AddReserveFundsLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const payoutDatum = decodePayoutDatum(config.payoutInput);
    const payoutDatumCbor = config.payoutInput.datum!;
    const payoutAssetName = payoutAssetNameFromInput(
      config.payoutInput,
      contracts.payout.policyId,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentPayoutAssets = removeAssetUnit(
      config.payoutInput.assets,
      payoutUnit,
      1n,
    );
    assertNoAssetExceeds(
      currentPayoutAssets,
      targetAssets,
      "Current payout input",
    );
    const neededAssets = subtractAssets(targetAssets, currentPayoutAssets);
    assertAssetsNonNegative(neededAssets, "Payout needed value");
    const takenAssets = minPositiveAssets(
      config.reserveInput.assets,
      neededAssets,
    );
    if (Object.keys(takenAssets).length === 0) {
      return yield* fail(
        "Reserve input does not contribute to any still-needed payout asset",
        {
          reserveInput: outRefLabel(config.reserveInput),
          neededAssets,
        },
      );
    }
    const payoutOutputAssets = addAssets(
      config.payoutInput.assets,
      takenAssets,
    );
    const reserveChangeAssets = subtractAssets(
      config.reserveInput.assets,
      takenAssets,
    );
    assertAssetsNonNegative(reserveChangeAssets, "Reserve change value");
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "reserve spending", script: contracts.reserve.spendingScript },
        { name: "payout spending", script: contracts.payout.spendingScript },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.payoutInput,
      config.reserveInput,
      hubOracleRefInput,
      ...(refs.reserveSpending === undefined ? [] : [refs.reserveSpending]),
      ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
    ]);
    const txInputs = [config.payoutInput, config.reserveInput, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      refs.reserveSpending,
      refs.payoutSpending,
    ]);
    const initialLayout = initialAddReserveFundsLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      payoutInput: config.payoutInput,
      reserveInput: config.reserveInput,
      hubOracleRefInput,
      reserveChangeAssets,
    });
    const makeTx = (layout: AddReserveFundsLayout): TxBuilder => {
      const payoutSpendRedeemer: SDK.PayoutSpendRedeemer = {
        AddFunds: {
          payout_input_index: layout.payoutInputIndex,
          payout_output_index: layout.payoutOutputIndex,
          reserve_input_index: layout.reserveInputIndex,
          reserve_change_output_index: layout.reserveChangeOutputIndex,
          reserve_spend_redeemer_index: layout.reserveSpendRedeemerIndex,
          payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      const reserveSpendRedeemer: SDK.ReserveSpendRedeemer = {
        reserve_input_index: layout.reserveInputIndex,
        payout_input_index: layout.payoutInputIndex,
        payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.payoutInput],
          Data.to(payoutSpendRedeemer, SDK.PayoutSpendRedeemer),
        )
        .collectFrom(
          [config.reserveInput],
          Data.to(reserveSpendRedeemer, SDK.ReserveSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .pay.ToAddressWithData(
          contracts.payout.spendingScriptAddress,
          { kind: "inline", value: payoutDatumCbor },
          payoutOutputAssets,
        );
      if (Object.keys(normalizeAssets(reserveChangeAssets)).length > 0) {
        tx = tx.pay.ToAddress(
          contracts.reserve.spendingScriptAddress,
          reserveChangeAssets,
        );
      }
      tx = attachIfMissing(
        tx,
        contracts.payout.spendingScript,
        refs.payoutSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.reserve.spendingScript,
        refs.reserveSpending,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "reserve funding",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveAddReserveFundsLayout({
          tx,
          payoutInput: config.payoutInput,
          reserveInput: config.reserveInput,
          payoutAddress: contracts.payout.spendingScriptAddress,
          payoutOutputAssets,
          payoutDatumCbor,
          reserveAddress: contracts.reserve.spendingScriptAddress,
          reserveChangeAssets,
          hubOracleRefInput,
        }),
      sameLayout: sameAddReserveFundsLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(`Reserve funding layout: ${formatLayout(built.layout)}`),
    ),
  );

export const buildConcludePayoutTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: ConcludePayoutConfig,
): Effect.Effect<
  BuiltReservePayoutTx<ConcludePayoutLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const payoutDatum = decodePayoutDatum(config.payoutInput);
    const payoutAssetName = payoutAssetNameFromInput(
      config.payoutInput,
      contracts.payout.policyId,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, payoutAssetName);
    const l1Assets = valueToAssets(payoutDatum.l2_value);
    const currentPayoutAssets = removeAssetUnit(
      config.payoutInput.assets,
      payoutUnit,
      1n,
    );
    if (!assetsEqual(currentPayoutAssets, l1Assets)) {
      return yield* fail(
        "Payout input value does not exactly equal the payout datum target",
        {
          payoutInput: outRefLabel(config.payoutInput),
          currentPayoutAssets,
          targetAssets: l1Assets,
        },
      );
    }
    const l1Address = addressDataToBech32(network, payoutDatum.l1_address);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        { name: "payout spending", script: contracts.payout.spendingScript },
        { name: "payout minting", script: contracts.payout.mintingScript },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.payoutInput,
      hubOracleRefInput,
      ...(refs.payoutSpending === undefined ? [] : [refs.payoutSpending]),
      ...(refs.payoutMinting === undefined ? [] : [refs.payoutMinting]),
    ]);
    const txInputs = [config.payoutInput, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      refs.payoutSpending,
      refs.payoutMinting,
    ]);
    const initialLayout = initialConcludePayoutLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      payoutInput: config.payoutInput,
      hubOracleRefInput,
    });
    const makeTx = (layout: ConcludePayoutLayout): TxBuilder => {
      const payoutSpendRedeemer: SDK.PayoutSpendRedeemer = {
        ConcludeWithdrawal: {
          payout_input_index: layout.payoutInputIndex,
          l1_output_index: layout.l1OutputIndex,
          burn_redeemer_index: layout.burnRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      const payoutBurnRedeemer: SDK.PayoutMintRedeemer = {
        BurnPayout: {
          payout_input_index: layout.payoutInputIndex,
          payout_asset_name: payoutAssetName,
          payout_spend_redeemer_index: layout.payoutSpendRedeemerIndex,
          hub_ref_input_index: layout.hubRefInputIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.payoutInput],
          Data.to(payoutSpendRedeemer, SDK.PayoutSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [payoutUnit]: -1n },
          Data.to(payoutBurnRedeemer, SDK.PayoutMintRedeemer),
        );
      tx = payToAddressWithCardanoDatum(
        tx,
        l1Address,
        payoutDatum.l1_datum,
        l1Assets,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.spendingScript,
        refs.payoutSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.payout.mintingScript,
        refs.payoutMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "payout conclusion",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveConcludePayoutLayout({
          tx,
          payoutInput: config.payoutInput,
          l1Address,
          l1Datum: payoutDatum.l1_datum,
          l1Assets,
          hubOracleRefInput,
        }),
      sameLayout: sameConcludePayoutLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(`Payout conclusion layout: ${formatLayout(built.layout)}`),
    ),
  );

export const buildRefundInvalidWithdrawalTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: RefundInvalidWithdrawalConfig,
): Effect.Effect<
  BuiltReservePayoutTx<RefundWithdrawalLayout>,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const network = yield* requireNetwork(lucid);
    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
      config.hubOracleRefInput,
    );
    const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
      config.withdrawal.assetName,
    );
    const resolvedReferenceScripts = yield* resolveReferenceScriptsProgram(
      lucid,
      config.referenceScriptsAddress,
      [
        {
          name: "withdrawal minting",
          script: contracts.withdrawal.mintingScript,
        },
        {
          name: "withdrawal spending",
          script: contracts.withdrawal.spendingScript,
        },
        { name: "withdrawal witness certificate", script: witnessScript },
        {
          name: "membership proof withdrawal",
          script: config.membershipProofWithdrawal.script,
        },
      ],
      config.referenceScripts,
    );
    const refs = mergeReferenceScripts(
      config.referenceScripts,
      resolvedReferenceScripts,
    );
    const withdrawalUnit = toUnit(
      contracts.withdrawal.policyId,
      config.withdrawal.assetName,
    );
    const refundAssets = removeAssetUnit(
      config.withdrawal.utxo.assets,
      withdrawalUnit,
      1n,
    );
    const refundAddress = addressDataToBech32(
      network,
      config.withdrawal.datum.refund_address,
    );
    const settlementDatum = settlementDatumFromInput(config.settlementRefInput);
    const overriddenWithdrawalInfo: SDK.WithdrawalInfo = {
      ...config.withdrawal.datum.event.info,
      validity: config.validityOverride,
    };
    const membershipRedeemer = encodeMembershipProofWithdrawalRedeemer(
      settlementDatum.withdrawals_root,
      Data.to(config.withdrawal.datum.event.id, SDK.OutputReference),
      Data.to(overriddenWithdrawalInfo, SDK.WithdrawalInfo),
      config.membershipProof,
    );
    const witnessRedeemer = SDK.encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.withdrawal.policyId,
    );
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      config.withdrawal.utxo,
      config.settlementRefInput,
      hubOracleRefInput,
      ...(refs.withdrawalMinting === undefined ? [] : [refs.withdrawalMinting]),
      ...(refs.withdrawalSpending === undefined
        ? []
        : [refs.withdrawalSpending]),
      ...(refs.withdrawalWitnessCertificate === undefined
        ? []
        : [refs.withdrawalWitnessCertificate]),
      ...(refs.membershipProofWithdrawal === undefined
        ? []
        : [refs.membershipProofWithdrawal]),
    ]);
    const txInputs = [config.withdrawal.utxo, feeInput];
    const txReferenceInputs = referenceInputs(hubOracleRefInput, [
      config.settlementRefInput,
      refs.withdrawalMinting,
      refs.withdrawalSpending,
      refs.withdrawalWitnessCertificate,
      refs.membershipProofWithdrawal,
    ]);
    const initialLayout = initialRefundWithdrawalLayout({
      inputs: txInputs,
      referenceInputs: txReferenceInputs,
      withdrawal: config.withdrawal,
      hubOracleRefInput,
      settlementRefInput: config.settlementRefInput,
    });
    const makeTx = (layout: RefundWithdrawalLayout): TxBuilder => {
      const withdrawalSpendRedeemer: SDK.WithdrawalSpendRedeemer = {
        input_index: layout.withdrawalInputIndex,
        output_index: layout.refundOutputIndex,
        hub_ref_input_index: layout.hubRefInputIndex,
        settlement_ref_input_index: layout.settlementRefInputIndex,
        burn_redeemer_index: layout.burnRedeemerIndex,
        payout_mint_redeemer_index: 0n,
        membership_proof: config.membershipProof,
        inclusion_proof_script_withdraw_redeemer_index:
          layout.inclusionProofWithdrawalRedeemerIndex,
        purpose: {
          Refund: {
            validity_override: config.validityOverride,
          },
        },
      };
      const withdrawalBurnRedeemer: SDK.WithdrawalMintRedeemer = {
        BurnEventNFT: {
          nonce_asset_name: config.withdrawal.assetName,
          witness_unregistration_redeemer_index:
            layout.witnessUnregistrationRedeemerIndex,
        },
      };
      let tx = lucid
        .newTx()
        .collectFrom(
          [config.withdrawal.utxo],
          Data.to(withdrawalSpendRedeemer, SDK.WithdrawalSpendRedeemer),
        )
        .collectFrom([feeInput])
        .readFrom([...txReferenceInputs])
        .mintAssets(
          { [withdrawalUnit]: -1n },
          Data.to(withdrawalBurnRedeemer, SDK.WithdrawalMintRedeemer),
        );
      tx = payToAddressWithCardanoDatum(
        tx,
        refundAddress,
        config.withdrawal.datum.refund_datum,
        refundAssets,
      );
      tx = applyEventWitnessUnregistration(
        tx,
        network,
        witnessScript,
        witnessRedeemer,
        refs.withdrawalWitnessCertificate,
      );
      tx = applyMembershipProofWithdrawal(
        tx,
        network,
        config.membershipProofWithdrawal,
        membershipRedeemer,
        refs.membershipProofWithdrawal,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.spendingScript,
        refs.withdrawalSpending,
      );
      tx = attachIfMissing(
        tx,
        contracts.withdrawal.mintingScript,
        refs.withdrawalMinting,
      );
      return tx;
    };
    return yield* completeWithTwoPassLayoutProgram({
      label: "invalid withdrawal refund",
      lucid,
      initialLayout,
      walletInputExclusions: [...txInputs, ...txReferenceInputs],
      makeTx,
      deriveLayout: (tx) =>
        deriveRefundWithdrawalLayout({
          tx,
          withdrawal: config.withdrawal,
          refundAddress,
          refundDatum: config.withdrawal.datum.refund_datum,
          refundAssets,
          hubOracleRefInput,
          settlementRefInput: config.settlementRefInput,
        }),
      sameLayout: sameRefundWithdrawalLayout,
    });
  }).pipe(
    Effect.tap((built) =>
      Effect.logInfo(
        `Invalid withdrawal refund layout: ${formatLayout(built.layout)}`,
      ),
    ),
  );

export const submitAbsorbConfirmedDepositToReserveProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AbsorbConfirmedDepositConfig,
): Effect.Effect<
  string,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const built = yield* buildAbsorbConfirmedDepositToReserveTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* handleSignSubmit(lucid, built.tx);
  });

export const submitInitializePayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: InitializePayoutConfig,
): Effect.Effect<
  string,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const built = yield* buildInitializePayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* handleSignSubmit(lucid, built.tx);
  });

export const submitAddReserveFundsToPayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: AddReserveFundsConfig,
): Effect.Effect<
  string,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const built = yield* buildAddReserveFundsToPayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* handleSignSubmit(lucid, built.tx);
  });

export const submitConcludePayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: ConcludePayoutConfig,
): Effect.Effect<
  string,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const built = yield* buildConcludePayoutTxProgram(lucid, contracts, config);
    return yield* handleSignSubmit(lucid, built.tx);
  });

export const submitRefundInvalidWithdrawalProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: RefundInvalidWithdrawalConfig,
): Effect.Effect<
  string,
  | ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const built = yield* buildRefundInvalidWithdrawalTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* handleSignSubmit(lucid, built.tx);
  });

export const __reservePayoutTest = {
  addAssets,
  assetsToValue,
  assetsEqual,
  encodeMembershipProofWithdrawalRedeemer,
  initialAbsorbDepositLayout,
  initialAddReserveFundsLayout,
  initialConcludePayoutLayout,
  initialInitializePayoutLayout,
  initialRefundWithdrawalLayout,
  aikenSerialisedPlutusDataCbor,
  minPositiveAssets,
  removeAssetUnit,
  selectFeeInputProgram,
  sameAddReserveFundsLayout,
  sameConcludePayoutLayout,
  subtractAssets,
  valueToAssets,
};
