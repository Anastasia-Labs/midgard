/**
 * The emulator's side of the follower harness (`follower-emulator.ts`): its
 * ledger as the local node encodes it (UTxO answers, Conway protocol
 * parameters, blocks of exact transaction bytes) and the node transport
 * double over it.
 */
import {
  CborMap,
  CborTag,
  encodeCbor,
  type LedgerQuery,
} from "@al-ft/l1-node-transport";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborMapRaw,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { decodeBlock, type FactStore } from "@al-ft/midgard-l1-follower";
import { decodeProtocolParameters } from "@al-ft/midgard-l1-follower/provider";
import {
  CML,
  Emulator,
  getAddressDetails,
  type ProtocolParameters,
  type UTxO,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";

type EmulatorEntry = { utxo: UTxO; spent: boolean };
type EmulatorChainAccount = {
  registeredStake: boolean;
  delegation: { poolId: string | null; rewards: bigint };
};
export type EmulatorState = {
  ledger: Record<string, EmulatorEntry>;
  mempool: Record<string, EmulatorEntry>;
  chain: Record<string, EmulatorChainAccount>;
  transactionHistory: Record<string, { status: string }>;
  protocolParameters: ProtocolParameters;
  slot: number;
};

export const stateOf = (emulator: Emulator): EmulatorState =>
  emulator as unknown as EmulatorState;

/** One `[outRef => output]` ledger answer, as local state query encodes it. */
export const utxoAnswer = (utxos: readonly UTxO[]): Buffer =>
  encodeCborMapRaw(
    utxos.map((utxo) => {
      const output = utxoToTransactionOutput(utxo);
      try {
        return [
          encodeCborArrayRaw([
            encodeCborBytes(Buffer.from(utxo.txHash, "hex")),
            encodeCborUnsigned(BigInt(utxo.outputIndex)),
          ]),
          Buffer.from(output.to_cbor_bytes()),
        ] as const;
      } finally {
        output.free();
      }
    }),
  );

/** A number the protocol parameters carry as a rational, exactly as Lucid holds it. */
const rational = (value: number): CborTag => {
  const denominator = 10n ** 12n;
  return new CborTag(30n, [BigInt(Math.round(value * 1e12)), denominator]);
};

const costModel = (model: unknown): bigint[] =>
  (Array.isArray(model)
    ? (model as number[])
    : Object.values(model as Record<string, number>)
  ).map((cost) => BigInt(cost));

/** The emulator's protocol parameters as the node's `protocol_params` answer (Conway order). */
export const ledgerProtocolParametersOf = (
  parameters: ProtocolParameters,
): Uint8Array => {
  const fields: Parameters<typeof encodeCbor>[0][] = Array.from(
    { length: 31 },
    () => 0n,
  );
  const version = parameters as ProtocolParameters & {
    protocolMajorVersion?: number;
    protocolMinorVersion?: number;
  };
  fields[0] = BigInt(parameters.minFeeA);
  fields[1] = BigInt(parameters.minFeeB);
  fields[3] = BigInt(parameters.maxTxSize);
  fields[5] = parameters.keyDeposit;
  fields[6] = parameters.poolDeposit;
  fields[12] = [
    BigInt(version.protocolMajorVersion ?? 11),
    BigInt(version.protocolMinorVersion ?? 0),
  ];
  fields[14] = parameters.coinsPerUtxoByte;
  fields[15] = new CborMap([
    [0n, costModel(parameters.costModels.PlutusV1)],
    [1n, costModel(parameters.costModels.PlutusV2)],
    [2n, costModel(parameters.costModels.PlutusV3)],
  ]);
  fields[16] = [rational(parameters.priceMem), rational(parameters.priceStep)];
  fields[17] = [parameters.maxTxExMem, parameters.maxTxExSteps];
  fields[19] = BigInt(parameters.maxValSize);
  fields[20] = BigInt(parameters.collateralPercentage);
  fields[21] = BigInt(parameters.maxCollateralInputs);
  fields[27] = parameters.govActionDeposit;
  fields[28] = parameters.drepDeposit;
  fields[30] = rational(parameters.minFeeRefScriptCostPerByte);
  const encoded = encodeCbor(fields);
  const decoded = decodeProtocolParameters(encoded);
  for (const key of [
    "minFeeA",
    "minFeeB",
    "maxTxSize",
    "maxValSize",
    "keyDeposit",
    "poolDeposit",
    "drepDeposit",
    "govActionDeposit",
    "priceMem",
    "priceStep",
    "maxTxExMem",
    "maxTxExSteps",
    "coinsPerUtxoByte",
    "collateralPercentage",
    "maxCollateralInputs",
    "minFeeRefScriptCostPerByte",
  ] as const)
    if (String(decoded[key]) !== String(parameters[key]))
      throw new Error(
        `the ledger answer does not carry the emulator's ${key}: ${String(decoded[key])} vs ${String(parameters[key])}`,
      );
  return encoded;
};

/** A block of `transactions` (exact bytes) on `parent`, as the node serves it. */
export const encodeFollowerBlock = (
  transactions: readonly string[],
  parent: Readonly<{ hash: Buffer; height: number }>,
  slot: number,
): Buffer => {
  const frames = transactions.map((cbor) =>
    CML.Transaction.from_cbor_hex(cbor),
  );
  try {
    const bodyParts = [
      encodeCborArrayRaw(frames.map((tx) => tx.body().to_cbor_bytes())),
      encodeCborArrayRaw(frames.map((tx) => tx.witness_set().to_cbor_bytes())),
      encodeCborMapRaw(
        frames.flatMap((tx, index) => {
          const auxiliary = tx.auxiliary_data();
          return auxiliary === undefined
            ? []
            : [
                [
                  encodeCborUnsigned(BigInt(index)),
                  auxiliary.to_cbor_bytes(),
                ] as const,
              ];
        }),
      ),
      encodeCborArrayRaw(
        frames.flatMap((tx, index) =>
          tx.is_valid() ? [] : [encodeCborUnsigned(BigInt(index))],
        ),
      ),
    ];
    const bodyHash = computeHash32(
      Buffer.concat(bodyParts.map((part) => computeHash32(part))),
    );
    const headerBody = CML.HeaderBody.new(
      BigInt(parent.height + 1),
      BigInt(slot),
      CML.BlockHeaderHash.from_raw_bytes(parent.hash),
      CML.PublicKey.from_bytes(new Uint8Array(32).fill(1)),
      CML.VRFVkey.from_raw_bytes(new Uint8Array(32).fill(2)),
      CML.VRFCert.new(new Uint8Array(64).fill(3), new Uint8Array(80).fill(4)),
      BigInt(bodyParts.reduce((size, part) => size + part.length, 0)),
      CML.BlockBodyHash.from_raw_bytes(bodyHash),
      CML.OperationalCert.new(
        CML.KESVkey.from_raw_bytes(new Uint8Array(32).fill(5)),
        0n,
        0n,
        CML.Ed25519Signature.from_raw_bytes(new Uint8Array(64).fill(6)),
      ),
      CML.ProtocolVersion.new(10n, 0n),
    );
    const header = CML.Header.new(
      headerBody,
      CML.KESSignature.from_cbor_bytes(
        encodeCborBytes(new Uint8Array(448).fill(7)),
      ),
    );
    return encodeCborArrayRaw([header.to_cbor_bytes(), ...bodyParts]);
  } finally {
    for (const frame of frames) frame.free();
  }
};

export const transactionHash = (cbor: string): string => {
  const tx = CML.Transaction.from_cbor_hex(cbor);
  try {
    return CML.hash_transaction(tx.body()).to_hex();
  } finally {
    tx.free();
  }
};

/** The script payment credentials outputs pay to and the policies minted, as hex. */
export const trackedItemsOf = (
  block: ReturnType<typeof decodeBlock>,
): { credentials: string[]; policies: string[] } => {
  const credentials: string[] = [];
  const policies: string[] = [];
  for (const tx of block.txs) {
    for (const output of [
      ...tx.outputs,
      ...(tx.collateralReturn === null ? [] : [tx.collateralReturn]),
    ])
      if (output.paymentCredential?.isScript === true)
        credentials.push(output.paymentCredential.hash.toString("hex"));
    policies.push(...tx.mint.keys());
  }
  return { credentials, policies };
};

/** What the transport double reads and reports. */
export type EmulatorTransportInput = Readonly<{
  emulator: Emulator;
  store: FactStore;
  origin: Readonly<{ slot: number; hash: Buffer }>;
  /** The emulator's own `submitTx`, which validates and holds the transaction. */
  submitTx: (this: Emulator, cbor: string) => Promise<string>;
  /** An accepted submission's exact bytes, as hex. */
  accepted: (cbor: string) => void;
}>;

/**
 * The local node transport over the emulator: it answers `protocol_params`,
 * the UTxO queries and the reward-account queries from the emulator's ledger,
 * submits to the emulator, and answers mempool presence from its history.
 */
export const emulatorNodeTransport = ({
  emulator,
  store,
  origin,
  submitTx,
  accepted,
}: EmulatorTransportInput) => {
  const state = stateOf(emulator);
  const ledgerUtxos = (): UTxO[] =>
    Object.values(state.ledger).flatMap(({ utxo, spent }) =>
      spent ? [] : [utxo],
    );
  const transport = {
    get readiness() {
      return { ready: true as const, nodeToClientVersion: 16 };
    },
    query: async (query: LedgerQuery): Promise<Uint8Array> => {
      switch (query.query) {
        case "protocol_params":
          return ledgerProtocolParametersOf(state.protocolParameters);
        case "utxo_by_txin": {
          const wanted = new Set(
            query.txIns.map(({ txId, index }) => `${txId}${index.toString()}`),
          );
          return utxoAnswer(
            ledgerUtxos().filter(({ txHash, outputIndex }) =>
              wanted.has(`${txHash}${outputIndex.toString()}`),
            ),
          );
        }
        case "utxo_by_address": {
          const wanted = new Set(
            query.addresses.map((address) =>
              Buffer.from(address).toString("hex"),
            ),
          );
          return utxoAnswer(
            ledgerUtxos().filter(({ address }) =>
              wanted.has(getAddressDetails(address).address.hex),
            ),
          );
        }
        case "chain_point": {
          const cursor = await store.cursor();
          return encodeCbor([
            BigInt(cursor?.point.slot ?? origin.slot),
            cursor?.point.hash ?? origin.hash,
          ]);
        }
        case "chain_block_no": {
          const cursor = await store.cursor();
          return encodeCbor([1n, BigInt(cursor?.height ?? 0)]);
        }
        case "stake_deleg_deposits":
        case "filtered_delegations_and_rewards": {
          const accounts = query.credentials.flatMap((credential) => {
            const entry = Object.entries(state.chain).find(
              ([rewardAddress]) =>
                getAddressDetails(rewardAddress).stakeCredential?.hash ===
                credential.hash,
            );
            return entry === undefined || !entry[1].registeredStake
              ? []
              : [{ credential, account: entry[1] }];
          });
          const key = (credential: (typeof accounts)[number]["credential"]) => [
            credential.type === "Key" ? 0n : 1n,
            Buffer.from(credential.hash, "hex"),
          ];
          if (query.query === "stake_deleg_deposits")
            return encodeCbor(
              new CborMap(
                accounts.map(({ credential }) => [
                  key(credential),
                  state.protocolParameters.keyDeposit,
                ]),
              ),
            );
          return encodeCbor([
            new CborMap(
              accounts.flatMap(({ credential, account }) =>
                account.delegation.poolId === null
                  ? []
                  : [
                      [
                        key(credential),
                        Buffer.from(
                          CML.Ed25519KeyHash.from_bech32(
                            account.delegation.poolId,
                          ).to_raw_bytes(),
                        ),
                      ],
                    ],
              ),
            ),
            new CborMap(
              accounts.map(({ credential, account }) => [
                key(credential),
                account.delegation.rewards,
              ]),
            ),
          ]);
        }
        case "system_start":
        case "current_era":
        case "era_history":
          throw new Error(`the emulator's node was asked ${query.query}`);
      }
    },
    withLedgerState: async <T>(
      _at: unknown,
      use: (session: {
        query: (query: LedgerQuery) => Promise<Uint8Array>;
      }) => Promise<T>,
    ): Promise<T> => use({ query: (query) => transport.query(query) }),
    submit: async (bytes: Uint8Array) => {
      const cbor = Buffer.from(bytes).toString("hex");
      try {
        await submitTx.call(emulator, cbor);
      } catch (error) {
        return {
          accepted: false as const,
          rejection: Buffer.from(
            error instanceof Error ? error.message : String(error),
          ),
        };
      }
      accepted(cbor);
      return { accepted: true as const };
    },
    hasTx: async (txHash: string) =>
      state.transactionHistory[txHash]?.status === "pending",
  };
  return transport;
};
