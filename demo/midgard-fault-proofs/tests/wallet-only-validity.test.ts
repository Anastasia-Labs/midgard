/**
 * The wallet-only validity window: its length against the deployment
 * profiles' proof-time bound, the upper bound each wallet-only transaction
 * carries, and the ledger refusing a publication held past it.
 */
import {
  encodeMidgardFieldPreimage,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { Proof } from "@al-ft/midgard-sdk";
import {
  scriptDecodeScriptHash,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isPrerequisiteTurn } from "../src/fault-proof-turn.js";
import { publishFaultProofFieldCarriage } from "../src/field-opening.publish-fault-proof-field-carriage.js";
import { buildProofChunkPublication } from "../src/publish-proof-chunks.js";
import { resolveProverSigner } from "../src/runtime.js";
import {
  buildScriptDecodeBindTx,
  buildScriptDecodeStageTx,
  scriptDecodeStagingDatums,
  stageScriptDecodeBytes,
} from "../src/script-decode/index.js";
import { resolveTransitionTraceDataCarriage } from "../src/transition-trace/proof-carriage.js";
import {
  WALLET_ONLY_VALIDITY_MS,
  walletOnlyValidTo,
} from "../src/wallet-only-validity.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { network } from "./support/emulator/blueprints.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./support/script-decode-emulator.js";
import {
  expectNoValidity,
  expectWalletOnlyValidity,
} from "./support/wallet-only-validity.js";

/** The flat program 1.1.0 `(con unit ())`, PlutusV3 script bytes. */
const DECODABLE_SCRIPT = Buffer.from("450101004981", "hex");
const PLUTUS_V3 = 3;

/** A prover funded at its enterprise address, on a fresh emulator. */
const prover = async () => {
  const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: account.seedPhrase,
  });
  const emulator = new Emulator(
    [{ ...account, address: signer.address }],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, "Custom");
  signer.selectWallet(lucid);
  return { emulator, lucid, signer };
};

type Prover = Awaited<ReturnType<typeof prover>>;

/** The publication turn of one transition-trace data chunk. */
const transitionTraceChunkPublication = async ({ lucid, signer }: Prover) => {
  const turn = await resolveTransitionTraceDataCarriage({
    lucid,
    address: signer.proofMaterialsAddress,
    datums: [Data.to("aa".repeat(64))],
    ownOutputs: createOwnOutputJournal(),
    publish: true,
  });
  if (!isPrerequisiteTurn(turn))
    throw new Error("the chunk lookup returned no publication turn");
  return turn.transaction;
};

describe("WALLET_ONLY_VALIDITY_MS", () => {
  it("is at most half the worst-case proof-time bound of every shortest-maturity deployment profile", () => {
    const profiles = Object.entries(DEPLOYMENT_PROFILES);
    const shortest = Math.min(
      ...profiles.map(([, profile]) => profile.timing.block_maturity_ms),
    );
    const shortestProfiles = profiles.filter(
      ([, profile]) => profile.timing.block_maturity_ms === shortest,
    );
    expect(shortestProfiles.length).toBeGreaterThan(0);
    for (const [name, profile] of shortestProfiles) {
      // The worst-case proof-time bound is half of maturity
      // (`MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs`).
      const worstCaseProofTimeBoundMs = profile.timing.block_maturity_ms / 2;
      expect(WALLET_ONLY_VALIDITY_MS, name).toBeLessThanOrEqual(
        worstCaseProofTimeBoundMs / 2,
      );
    }
  });
});

describe("walletOnlyValidTo", () => {
  it("bounds a transaction built now at the slot one window on", async () => {
    const { emulator, lucid } = await prover();
    emulator.awaitSlot(7);
    const validTo = walletOnlyValidTo(lucid);
    expect(lucid.slotToUnixTime(lucid.unixTimeToSlot(validTo))).toBe(validTo);
    expect(validTo - lucid.slotToUnixTime(lucid.currentSlot())).toBe(
      WALLET_ONLY_VALIDITY_MS,
    );
  });
});

describe("wallet-only transactions", () => {
  const kinds: readonly (readonly [
    string,
    (fixture: Prover) => Promise<{
      readonly lucid: LucidEvolution;
      readonly cbor: string;
    }>,
  ])[] = [
    [
      "proof chunk publication",
      async ({ lucid, signer }) => {
        const { transaction } = await buildProofChunkPublication({
          lucid,
          signer,
          proofCbor: Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof),
        });
        return { lucid, cbor: transaction.signed.toCBOR() };
      },
    ],
    [
      "transition-trace data carriage",
      async (fixture) => ({
        lucid: fixture.lucid,
        cbor: (await transitionTraceChunkPublication(fixture)).signed.toCBOR(),
      }),
    ],
    [
      "script-decode stage",
      async ({ lucid, signer }) => {
        const built = await buildScriptDecodeStageTx({
          lucid,
          datum: scriptDecodeStagingDatums(DECODABLE_SCRIPT)[0]!,
          address: signer.proofMaterialsAddress,
        });
        return { lucid, cbor: built.signed.toCBOR() };
      },
    ],
    [
      "script-decode Bind",
      async () => {
        const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
        const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
        const deployment = injectScriptDecodeDeployment(emulator);
        const lucid = await Lucid(emulator, "Custom");
        lucid.selectWallet.fromSeed(account.seedPhrase);
        const staging = await stageScriptDecodeBytes({
          lucid,
          scriptBytes: DECODABLE_SCRIPT,
        });
        const built = await buildScriptDecodeBindTx({
          lucid,
          deployment,
          language: PLUTUS_V3,
          scriptHashHex: scriptDecodeScriptHash(
            PLUTUS_V3,
            DECODABLE_SCRIPT,
          ).toString("hex"),
          initialRun: startScriptDecodeWalk(PLUTUS_V3, DECODABLE_SCRIPT).run,
          staging,
        });
        return { lucid, cbor: built.signed.toCBOR() };
      },
    ],
  ];

  it.each(kinds)(
    "%s carries the wallet-only upper bound and no lower bound",
    async (_kind, build) => {
      const { lucid, cbor } = await build(await prover());
      expectWalletOnlyValidity(lucid, cbor);
    },
    120_000,
  );
});

describe("the field-preimage carriage publication", () => {
  // Exempt on purpose: a full chunk's publication already lands exactly on the
  // publication size target (`atKSigned` in midgard-validation's
  // field-preimage-carriage-fit-emulator.8-3-phase-4-exit-measurement test),
  // so a bound would not fit. Giving it one has to meet that measurement.
  it("carries no validity bound, since a full chunk's publication sits exactly on the publication size target", async () => {
    const { lucid, signer } = await prover();
    const preimage = encodeMidgardFieldPreimage(
      Array.from({ length: 100 }, (_, k) => Buffer.alloc(26, k)),
    );
    const plan = planMidgardFieldCarriage({
      owner: Buffer.from(signer.paymentKeyHash, "hex"),
      txId: Buffer.alloc(32, 7),
      fieldIndex: 2,
      preimage,
      publish: true,
    });
    expect(plan.tier).toBe("RawUtxo");
    const turn = await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned: {
        sourceKind: 0n,
        fieldIndex: plan.fieldIndex,
        nativeTxId: plan.txId.toString("hex"),
        nativeTxCompactCbor: "",
        preimage,
        itemCount: 100,
        commitment: plan.commitment.toString("hex"),
        plan,
      },
      publisherAddress: signer.proofMaterialsAddress,
      label: "field carriage",
      ownOutputs: createOwnOutputJournal(),
    });
    if (!isPrerequisiteTurn(turn))
      throw new Error("the carriage returned no publication turn");
    expectNoValidity(turn.transaction.signed.toCBOR());
  }, 120_000);
});

describe("a wallet-only publication against the ledger", () => {
  it("is refused once its window has passed and accepted inside it", async () => {
    const fixture = await prover();
    const { emulator, lucid } = fixture;

    const stale = await transitionTraceChunkPublication(fixture);
    emulator.awaitSlot(WALLET_ONLY_VALIDITY_MS / 1000 + 1);
    await expect(stale.signed.submit()).rejects.toThrow(
      /Upper bound \(\d+\) not in slot range/,
    );

    const fresh = await transitionTraceChunkPublication(fixture);
    emulator.awaitSlot(60);
    const txHash = await fresh.signed.submit();
    expect(await lucid.awaitTx(txHash)).toBe(true);
    expect(
      (await lucid.utxosAt(fixture.signer.proofMaterialsAddress)).map(
        (utxo) => utxo.txHash,
      ),
    ).toContain(txHash);
  }, 120_000);
});
