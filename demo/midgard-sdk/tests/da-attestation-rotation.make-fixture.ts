import { readFileSync } from "node:fs";

import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  type Assets,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  availabilityResponseGeometry,
  buildDaAvailabilityCommitment,
  type DaAttestationBuildError,
  DaAttestationDatum,
  type DaAttestationReferenceScripts,
  daAttestationUnit,
  type DaAttestationUtxo,
  type DaParamsDatum,
  type MidgardValidators,
} from "../src/index.js";

// Q62 (decision row D-DA4) and Q63 acceptance clause (c) (decision row D-DA5).
//
// This suite is the off-chain half. It cannot execute Plutus, so it never
// claims to prove the validator's behaviour — the Aiken family in
// `onchain/aiken/validators/da-attestation.ak` does that. What it does prove is
// that the two sides agree on the wire: the redeemer constructor ordering the
// validator decodes, and the rotation condition the builders refuse to ignore.

const repositoryRoot = new URL("../../../", import.meta.url);

const readRepositoryFile = (relativePath: string): string =>
  readFileSync(new URL(relativePath, repositoryRoot), "utf8");

/**
 * Constructor names of an Aiken sum type, in declaration order.
 *
 * Read out of the Aiken source rather than restated here. Plutus tags a
 * constructor by its *position*, so a reordering of the declaration silently
 * re-points every encoded redeemer at a different branch — a change no type
 * checker on either side would catch. Deriving the expected order from the
 * declaration is what makes the CBOR assertions below a cross-language pin
 * rather than a restatement of the TypeScript enum against itself.
 */
export const aikenConstructorOrder = (typeName: string): readonly string[] => {
  const source = readRepositoryFile(
    "onchain/aiken/lib/midgard/da-attestation-types.ak",
  );
  const opening = new RegExp(`^pub type ${typeName} \\{$`, "mu").exec(source);
  if (opening?.index === undefined) {
    throw new Error(
      `${typeName} is no longer declared in onchain/aiken/lib/midgard/da-attestation-types.ak`,
    );
  }
  const body = source.slice(opening.index + opening[0].length);
  const end = body.indexOf("\n}");
  if (end === -1) {
    throw new Error(`${typeName} has no closing brace`);
  }
  return [...body.slice(0, end).matchAll(/^ {2}([A-Z][A-Za-z0-9]*) \{/gmu)].map(
    (match) => match[1] as string,
  );
};

/**
 * Field names of one constructor of an Aiken sum type, in declaration order,
 * read from the same source as {@link aikenConstructorOrder}. Doc comments
 * between fields are skipped.
 */
export const aikenConstructorFields = (
  typeName: string,
  constructorName: string,
): readonly string[] => {
  const source = readRepositoryFile(
    "onchain/aiken/lib/midgard/da-attestation-types.ak",
  );
  const opening = new RegExp(`^pub type ${typeName} \\{$`, "mu").exec(source);
  if (opening?.index === undefined) {
    throw new Error(`${typeName} is no longer declared`);
  }
  const typeBody = source.slice(opening.index + opening[0].length);
  const lines = typeBody.slice(0, typeBody.indexOf("\n}")).split("\n");
  const header = `  ${constructorName} {`;
  const start = lines.findIndex((line) => line.startsWith(header));
  if (start === -1) {
    throw new Error(`${typeName} has no constructor ${constructorName}`);
  }
  const rest = lines[start]!.slice(header.length);
  if (rest.includes("}")) {
    return [...rest.matchAll(/([a-z_][a-z0-9_]*):/gu)].map(
      (match) => match[1] as string,
    );
  }
  const fields: string[] = [];
  for (const line of lines.slice(start + 1)) {
    if (line === "  }") {
      return fields;
    }
    const match = /^ {4}([a-z_][a-z0-9_]*):/u.exec(line);
    if (match !== null) {
      fields.push(match[1] as string);
    }
  }
  throw new Error(`${constructorName} has no closing brace`);
};

/** Plutus constructor tag for index `i` (i < 7): 121 + i, CBOR tag `d879 + i`. */
export const constructorTagPrefix = (index: number): string =>
  `d8${(0x79 + index).toString(16)}`;

export const availabilityCommitment = (headerHash: string) =>
  buildDaAvailabilityCommitment({
    deploymentIdentity: h28(0x71),
    headerHash,
    payload: Uint8Array.of(1),
    responseGeometry: availabilityResponseGeometry({
      chunkByteLength: 4096,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });

type RecordedPayment = {
  readonly address: string;
  readonly assets: Assets;
};

type Recording = {
  readonly reads: UTxO[][];
  readonly collects: { readonly inputs: UTxO[]; readonly redeemer: unknown }[];
  readonly mints: { readonly assets: Assets; readonly redeemer: unknown }[];
  readonly payments: RecordedPayment[];
};

export const makeRecordingLucid = (): {
  readonly lucid: LucidEvolution;
  readonly record: Recording;
} => {
  const record: Recording = {
    reads: [],
    collects: [],
    mints: [],
    payments: [],
  };
  const lucid = {
    newTx: () => {
      const tx = {
        readFrom: (inputs: UTxO[]) => {
          record.reads.push(inputs);
          return tx;
        },
        collectFrom: (inputs: UTxO[], redeemer: unknown) => {
          record.collects.push({ inputs, redeemer });
          return tx;
        },
        mintAssets: (assets: Assets, redeemer: unknown) => {
          record.mints.push({ assets, redeemer });
          return tx;
        },
        pay: {
          ToAddress: (address: string, assets: Assets) => {
            record.payments.push({ address, assets });
            return tx;
          },
        },
      };
      return tx;
    },
  } as unknown as LucidEvolution;
  return { lucid, record };
};

const makeUtxo = (
  outputIndex: number,
  assets: Assets = { lovelace: 1n },
  datum: string | null = null,
  address = `addr_test_${outputIndex.toString()}`,
): UTxO =>
  ({
    txHash: outputIndex.toString(16).padStart(64, "0"),
    outputIndex,
    address,
    assets,
    datum,
  }) as UTxO;

const validator = (policyByte: number, address: string) =>
  ({
    policyId: h28(policyByte),
    spendingScriptAddress: address,
    spendingScriptHash: h28(policyByte),
    spendingScriptCBOR: "",
    mintingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "" },
    mintingScript: { type: "PlutusV3", script: "" },
  }) as unknown as MidgardValidators["daAttestation"];

export const GOVERNED_COMMITTEE_HASH = h32(0x33);

export const ROTATED_COMMITTEE_HASH = h32(0x44);

export const makeFixture = () => {
  const contracts = {
    daAttestation: validator(0xaa, "addr_da_attestation"),
  } as Pick<MidgardValidators, "daAttestation">;
  const headerHash = h28(0x10);
  const daParamsDatum: DaParamsDatum = {
    committee: h32(0x11) + h32(0x22),
    committee_signers_hash: GOVERNED_COMMITTEE_HASH,
    da_threshold: 2n,
    owners: [h28(0x81), h28(0x82)],
    update_threshold: 2n,
  };
  const daParamsUtxo = makeUtxo(2, { lovelace: 2_000_000n });
  const attestationUnit = daAttestationUnit(
    contracts.daAttestation,
    headerHash,
  );
  // The stranded attestation: it froze a committee governance has since rotated
  // away from, and it holds one of the two signatures it needed.
  const attestationDatum: DaAttestationDatum = {
    header_hash: headerHash,
    availability_commitment: availabilityCommitment(headerHash),
    da_threshold: 2n,
    committee_signers_hash: ROTATED_COMMITTEE_HASH,
    rescue_beneficiary: {
      paymentCredential: { PublicKeyCredential: [h28(0x66)] },
      stakeCredential: null,
    },
    attested_signers: `80${"00".repeat(31)}`,
    attestation_count: 1n,
  };
  const attestation: DaAttestationUtxo = {
    utxo: makeUtxo(
      3,
      { lovelace: 5_000_000n, [attestationUnit]: 1n },
      Data.to(attestationDatum, DaAttestationDatum),
      contracts.daAttestation.spendingScriptAddress,
    ),
    datum: attestationDatum,
  };
  const referenceScripts: Pick<
    DaAttestationReferenceScripts,
    "daAttestationMinting" | "daAttestationSpending"
  > = {
    daAttestationMinting: makeUtxo(4),
    daAttestationSpending: makeUtxo(5),
  };
  return {
    contracts,
    headerHash,
    daParamsDatum,
    daParamsUtxo,
    attestation,
    attestationUnit,
    referenceScripts,
    refundAddress: credentialToAddress("Preprod", {
      type: "Key",
      hash: h28(0x66),
    }),
  };
};

export const run = <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<A> => Effect.runPromise(program);

export const expectBuildFailure = async <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<void> => {
  const result = await Effect.runPromise(Effect.either(program));
  expect(result._tag).toBe("Left");
  if (result._tag === "Left") {
    expect(result.left._tag).toBe("DaAttestationBuildError");
  }
};
