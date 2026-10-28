/**
 * The user-facing L1 builders (deposit, withdrawal and forced tx order) take
 * caller-supplied UTxOs: explicit wallet inputs, the retained external data
 * output, reference-script outputs, the forced order's nonce and its carriage
 * reference inputs. A malformed one must be refused by name, with the config
 * field it came from, before the builder touches contracts, the provider or
 * Lucid's transaction builder. The same holds for a wallet whose provider view
 * is malformed.
 *
 * The contracts are deliberately empty: every refusal below has to happen
 * before they are read. The controls show that well-formed inputs pass the
 * screen and fail later, on those empty contracts, for a different reason.
 */
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const emptyContracts = {} as SDK.MidgardValidators;
let lucid: LucidEvolution;
let wallet: UTxO;

beforeAll(async () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  lucid = await Lucid(new Emulator([account]), "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  [wallet] = await lucid.wallet().getUtxos();
});

const refusal = async <A, E>(program: Effect.Effect<A, E>) => {
  const result = await Effect.runPromise(Effect.either(program));
  if (Either.isRight(result)) throw new Error("builder unexpectedly succeeded");
  return result.left as unknown as {
    readonly _tag: string;
    readonly role?: string;
    readonly outRef?: string | null;
    readonly message: string;
  };
};

const policy = "ab".repeat(28);

/** One malformed variant per field a UTxO carries. */
const malformed: ReadonlyArray<
  readonly [string, (base: UTxO) => unknown, RegExp]
> = [
  ["a missing entry", () => null, /not a UTxO object/u],
  [
    "a short transaction hash",
    (base) => ({ ...base, txHash: "ab" }),
    /txHash/u,
  ],
  [
    "an uppercase transaction hash",
    (base) => ({ ...base, txHash: "AB".repeat(32) }),
    /txHash/u,
  ],
  [
    "a negative output index",
    (base) => ({ ...base, outputIndex: -1 }),
    /outputIndex/u,
  ],
  [
    "a fractional output index",
    (base) => ({ ...base, outputIndex: 1.5 }),
    /outputIndex/u,
  ],
  [
    "an output index beyond the ledger's 16 bits",
    (base) => ({ ...base, outputIndex: 65_536 }),
    /outputIndex/u,
  ],
  [
    "an address that is not a Cardano address",
    (base) => ({ ...base, address: "addr_test1notanaddress" }),
    /address/u,
  ],
  ["no asset map", (base) => ({ ...base, assets: undefined }), /assets/u],
  [
    "no lovelace",
    (base) => ({ ...base, assets: { [`${policy}01`]: 1n } }),
    /lovelace/u,
  ],
  [
    "a lovelace amount that is a number",
    (base) => ({ ...base, assets: { lovelace: 5_000_000 } }),
    /lovelace/u,
  ],
  [
    "a zero lovelace amount",
    (base) => ({ ...base, assets: { lovelace: 0n } }),
    /lovelace/u,
  ],
  [
    "a lovelace amount beyond 64 bits",
    (base) => ({ ...base, assets: { lovelace: 2n ** 64n } }),
    /lovelace/u,
  ],
  [
    "an asset unit that is not a policy id and asset name",
    (base) => ({ ...base, assets: { ...base.assets, abc: 1n } }),
    /asset unit/u,
  ],
  [
    "an asset name longer than 32 bytes",
    (base) => ({
      ...base,
      assets: { ...base.assets, [`${policy}${"00".repeat(33)}`]: 1n },
    }),
    /asset unit/u,
  ],
  [
    "a zero token quantity",
    (base) => ({ ...base, assets: { ...base.assets, [`${policy}01`]: 0n } }),
    /quantity/u,
  ],
  [
    "a malformed datum hash",
    (base) => ({ ...base, datumHash: "00" }),
    /datumHash/u,
  ],
  [
    "a datum that is not hex CBOR",
    (base) => ({ ...base, datum: "not-cbor" }),
    /datum/u,
  ],
  [
    "a reference script of an unknown language",
    (base) => ({ ...base, scriptRef: { type: "PlutusV9", script: "00" } }),
    /scriptRef/u,
  ],
  [
    "a reference script with no bytes",
    (base) => ({ ...base, scriptRef: { type: "PlutusV3", script: "" } }),
    /scriptRef/u,
  ],
];

const depositConfig = (
  overrides: Partial<SDK.SubmitDepositConfig>,
): SDK.SubmitDepositConfig => ({
  l2Address: "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz",
  l2Datum: null,
  lovelace: 5_000_000n,
  additionalAssets: {},
  ...overrides,
});

const withdrawalConfig = (
  overrides: Partial<SDK.SubmitWithdrawalConfig>,
): SDK.SubmitWithdrawalConfig =>
  ({
    bodyCbor: "00",
    signature: { key: "00".repeat(32), signature: "00".repeat(64) },
    refundAddress: {
      paymentCredential: { PublicKeyCredential: ["00".repeat(28)] },
      stakeCredential: null,
    },
    ...overrides,
  }) as SDK.SubmitWithdrawalConfig;

const txOrderConfig = (
  overrides: Partial<SDK.SubmitTxOrderConfig>,
): SDK.SubmitTxOrderConfig => ({
  submittedTxCbor: "00",
  nonceInput: wallet,
  refundAddress: {
    paymentCredential: { PublicKeyCredential: ["00".repeat(28)] },
    stakeCredential: null,
  },
  ...overrides,
});

describe("deposit builder refuses malformed UTxOs by name", () => {
  it.each(malformed)(
    "refuses an explicit wallet input with %s",
    async (_label, corrupt, reason) => {
      const error = await refusal(
        SDK.unsignedDepositTxProgram(
          lucid,
          emptyContracts,
          depositConfig({ walletInputs: [wallet, corrupt(wallet) as UTxO] }),
        ),
      );
      expect(error._tag).toBe("InvalidUtxoError");
      expect(error).toBeInstanceOf(SDK.InvalidUtxoError);
      expect(error.role).toBe("walletInputs[1]");
      expect(error.message).toMatch(reason);
    },
  );

  it("refuses the same output listed twice in the wallet inputs", async () => {
    const error = await refusal(
      SDK.unsignedDepositTxProgram(
        lucid,
        emptyContracts,
        depositConfig({ walletInputs: [wallet, { ...wallet }] }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("walletInputs[1]");
    expect(error.outRef).toBe(`${wallet.txHash}#${wallet.outputIndex}`);
    expect(error.message).toMatch(/twice/u);
  });

  it("refuses a malformed retained external data output", async () => {
    const error = await refusal(
      SDK.unsignedDepositTxProgram(
        lucid,
        emptyContracts,
        depositConfig({
          walletInputs: [wallet],
          externalData: { ...wallet, outputIndex: -2 },
        }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("externalData");
  });

  it("refuses a malformed minting reference-script output", async () => {
    const error = await refusal(
      SDK.unsignedDepositTxProgram(
        lucid,
        emptyContracts,
        depositConfig({
          walletInputs: [wallet],
          referenceScripts: {
            depositMinting: { ...wallet, assets: {} },
          },
        }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("referenceScripts.depositMinting");
  });

  it("refuses a provider wallet view holding a malformed UTxO", async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const partial = await Lucid(new Emulator([account]), "Custom");
    const [coin] = await partial.utxosAt(account.address);
    partial.selectWallet.fromAddress(account.address, [
      coin!,
      { ...coin!, outputIndex: 1, assets: undefined } as unknown as UTxO,
    ]);
    const error = await refusal(
      SDK.unsignedDepositTxProgram(partial, emptyContracts, depositConfig({})),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("wallet[1]");
  });

  it("passes well-formed inputs on to the contracts", async () => {
    const error = await refusal(
      SDK.unsignedDepositTxProgram(
        lucid,
        emptyContracts,
        depositConfig({
          walletInputs: [wallet],
          externalData: wallet,
          referenceScripts: { depositMinting: wallet },
        }),
      ),
    );
    expect(error._tag).not.toBe("InvalidUtxoError");
  });
});

describe("withdrawal builder refuses malformed UTxOs by name", () => {
  it("refuses a malformed explicit wallet input", async () => {
    const error = await refusal(
      SDK.unsignedWithdrawalTxProgram(
        lucid,
        emptyContracts,
        withdrawalConfig({
          walletInputs: [{ ...wallet, txHash: "zz".repeat(32) }],
        }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("walletInputs[0]");
  });

  it("refuses a malformed minting reference-script output", async () => {
    const error = await refusal(
      SDK.unsignedWithdrawalTxProgram(
        lucid,
        emptyContracts,
        withdrawalConfig({
          walletInputs: [wallet],
          referenceScripts: {
            withdrawalMinting: {
              ...wallet,
              scriptRef: { type: "PlutusV3", script: "xyz" },
            },
          },
        }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("referenceScripts.withdrawalMinting");
  });

  it("passes well-formed inputs on to the contracts", async () => {
    const error = await refusal(
      SDK.unsignedWithdrawalTxProgram(
        lucid,
        emptyContracts,
        withdrawalConfig({ walletInputs: [wallet] }),
      ),
    );
    expect(error._tag).not.toBe("InvalidUtxoError");
  });
});

describe("forced tx-order builder refuses malformed UTxOs by name", () => {
  it.each(malformed)(
    "refuses a nonce input with %s",
    async (_label, corrupt, reason) => {
      const error = await refusal(
        SDK.unsignedTxOrderTxProgram(
          lucid,
          emptyContracts,
          txOrderConfig({ nonceInput: corrupt(wallet) as UTxO }),
        ),
      );
      expect(error._tag).toBe("InvalidUtxoError");
      expect(error.role).toBe("nonceInput");
      expect(error.message).toMatch(reason);
    },
  );

  it("refuses a malformed carriage reference input", async () => {
    const error = await refusal(
      SDK.unsignedTxOrderTxProgram(
        lucid,
        emptyContracts,
        txOrderConfig({
          carriageReferenceInputs: [wallet, { ...wallet, datum: "0" }],
        }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("carriageReferenceInputs[1]");
  });

  it("refuses a malformed minting reference-script output", async () => {
    const error = await refusal(
      SDK.unsignedTxOrderTxProgram(
        lucid,
        emptyContracts,
        txOrderConfig({
          referenceScripts: {
            txOrderMinting: { ...wallet, address: "" },
          },
        }),
      ),
    );
    expect(error._tag).toBe("InvalidUtxoError");
    expect(error.role).toBe("referenceScripts.txOrderMinting");
  });

  it("passes well-formed inputs on to the contracts", async () => {
    const error = await refusal(
      SDK.unsignedTxOrderTxProgram(
        lucid,
        emptyContracts,
        txOrderConfig({
          carriageReferenceInputs: [wallet],
          referenceScripts: { txOrderMinting: wallet },
        }),
      ),
    );
    expect(error._tag).not.toBe("InvalidUtxoError");
  });
});
