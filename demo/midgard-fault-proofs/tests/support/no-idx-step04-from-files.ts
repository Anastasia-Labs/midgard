/**
 * The files a no-idx step-04 `submit-*-step-04` command reads, written the way
 * an operator hands them over: the blueprint, the deployment info (with or
 * without the field grammar certificate members), the producer's outputs
 * preimage, its compact transaction and its compact witness set.
 */
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { decodeMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  FIELD_GRAMMAR_CERTIFICATE_MINT,
  FIELD_GRAMMAR_CERTIFICATE_SPEND,
} from "../../src/field-grammar-certificate.js";
import { stringifyJson } from "../../src/json-file.js";

/** The deployment info with both certificate members naming `utxo`. */
const withFieldGrammarMembers = (
  deploymentInfo: Readonly<Record<string, unknown>>,
  members: Readonly<{
    referenceScriptUtxo: UTxO;
    spendScriptHash: string;
    policyId: string;
  }>,
) => {
  const refScriptUTxO = {
    txHash: members.referenceScriptUtxo.txHash,
    outputIndex: members.referenceScriptUtxo.outputIndex,
  };
  return {
    ...deploymentInfo,
    contracts: {
      ...(deploymentInfo.contracts as Record<string, unknown>),
      [FIELD_GRAMMAR_CERTIFICATE_SPEND]: {
        scriptHash: members.spendScriptHash,
        refScriptUTxO,
      },
      [FIELD_GRAMMAR_CERTIFICATE_MINT]: {
        scriptHash: members.policyId,
        refScriptUTxO,
      },
    },
  };
};

export const writeNoIdxStep04Files = async ({
  blueprint,
  deploymentInfo,
  fieldGrammarMembers,
  outputsPreimageCbor,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: Readonly<Record<string, unknown>>;
  readonly fieldGrammarMembers?: Parameters<typeof withFieldGrammarMembers>[1];
  readonly outputsPreimageCbor: readonly string[];
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
}) => {
  const directory = await mkdtemp(join(tmpdir(), "no-idx-step04-files-"));
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  const files = {
    blueprintPath: blueprint,
    deploymentInfoPath:
      fieldGrammarMembers === undefined
        ? deploymentInfo
        : withFieldGrammarMembers(deploymentInfo, fieldGrammarMembers),
    outputsPreimagePath: { outputsPreimageCbor },
    nativeTxCompactPath: { nativeTxCompactCbor },
    witnessSetCompactPath: {
      addrTxWitsHash: witnessSet.addrTxWitsHash.toString("hex"),
      scriptTxWitsHash: witnessSet.scriptTxWitsHash.toString("hex"),
      redeemerTxWitsHash: witnessSet.redeemerTxWitsHash.toString("hex"),
    },
  } as const;
  const paths = Object.fromEntries(
    await Promise.all(
      Object.entries(files).map(async ([key, value]) => {
        const path = join(directory, `${key}.json`);
        await writeFile(path, stringifyJson(value));
        return [key, path] as const;
      }),
    ),
  ) as Record<keyof typeof files, string>;
  return paths;
};
