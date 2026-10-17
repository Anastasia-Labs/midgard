import { normalizeHex } from "@al-ft/midgard-core/hex";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  applyBlueprintParams,
  getBlueprintValidator,
  getUnappliedScript,
  MerkleRoot,
  parseFaultProofBlueprint,
  Proof,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type Network,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { aikenSerialisedPlutusDataCbor } from "./plutus-data-cbor.js";

export const resolveFraudulentHeaderHash = ({
  stateQueuePolicyId,
  fraudulentBlockUtxo,
  configuredHeaderHash,
}: {
  readonly stateQueuePolicyId: string;
  readonly fraudulentBlockUtxo: UTxO;
  readonly configuredHeaderHash?: string;
}): string => {
  const prefix = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}`;
  const candidates = Object.entries(fraudulentBlockUtxo.assets)
    .filter(
      ([unit, amount]) =>
        amount === 1n &&
        unit.startsWith(prefix) &&
        unit.length === prefix.length + 56,
    )
    .map(([unit]) => unit.slice(prefix.length));
  if (candidates.length !== 1) {
    throw new Error(
      `Expected fraudulent block UTxO ${outRefLabel(fraudulentBlockUtxo)} to carry exactly one state-queue block token for policy ${stateQueuePolicyId}, found ${candidates.length.toString()}.`,
    );
  }
  const derived = candidates[0]!;
  const configured =
    configuredHeaderHash === undefined
      ? undefined
      : normalizeHex(configuredHeaderHash, {
          fieldName: "--fraudulent-header-hash",
          byteLength: 28,
        });
  if (configured !== undefined && configured !== derived) {
    throw new Error(
      `--fraudulent-header-hash mismatch: provided=${configured}, derived=${derived}.`,
    );
  }
  return derived;
};

export const phasMembershipRewardAddress = (
  network: Network,
  script: Script,
): string => {
  const networkId = network === "Mainnet" ? 1 : 0;
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(networkId, credential).to_address().to_bech32();
};

/** Deployable bare scripts must declare no parameters. */
export const getCompiledScript = (blueprint: unknown, title: string): string =>
  getUnappliedScript(parseFaultProofBlueprint(blueprint), title);

/** Runtime JSON adapter to the SDK's strict parameter application boundary. */
export const applyBlueprintParamsExact = ({
  blueprint,
  title,
  params,
}: {
  readonly blueprint: unknown;
  readonly title: string;
  readonly params: readonly Data[];
}): string =>
  applyBlueprintParams(parseFaultProofBlueprint(blueprint), title, params);

/**
 * Measure an unapplied blueprint body without deploying it. The caller must
 * pin the expected declared arity so an accidental parameter-shape change
 * fails alongside the byte-size measurement.
 */
export const measureBlueprintValidatorBytes = ({
  blueprint,
  title,
  expectedDeclaredParameterCount,
}: {
  readonly blueprint: unknown;
  readonly title: string;
  readonly expectedDeclaredParameterCount: number;
}): number => {
  const found = getBlueprintValidator(
    parseFaultProofBlueprint(blueprint),
    title,
  );
  if (found.parameters.length !== expectedDeclaredParameterCount) {
    throw new Error(
      `${title} declares ${found.parameters.length.toString()} parameter(s), not the measured invariant ${expectedDeclaredParameterCount.toString()}.`,
    );
  }
  return found.compiledCode.length / 2;
};

export const encodePhasMembershipProofRedeemer = ({
  root,
  keyCbor,
  valueCbor,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly keyCbor: string;
  readonly valueCbor: string;
  readonly membershipProofCbor: string;
}): string => {
  const proof = Data.from(membershipProofCbor, Proof);
  const rootData = Data.from(Data.to(root, MerkleRoot));
  const keyData = Data.from(
    Data.to(
      aikenSerialisedPlutusDataCbor(keyCbor),
      asLucidSchema(Data.Bytes()),
    ),
  );
  const valueData = Data.from(
    Data.to(
      aikenSerialisedPlutusDataCbor(valueCbor),
      asLucidSchema(Data.Bytes()),
    ),
  );
  const proofData = Data.from(Data.to(proof, asLucidSchema(Proof)));
  return Data.to(
    [rootData, keyData, valueData, proofData],
    asLucidSchema(Data.Array(Data.Any())),
  );
};

/**
 * Non-membership (exclusion) redeemer for the `pexcludes.exclusion.withdraw`
 * validator: `[root, key, proof]` (no value, unlike the phas membership
 * counterpart). `keyBytes` are the trie's native key bytes — the ledger trie
 * keyed by Cardano `TransactionInput` CBOR, or the transactions trie keyed by
 * the raw 32-byte native tx id.
 */
export const encodeRawPexcludesProofRedeemer = ({
  root,
  keyBytes,
  nonMembershipProofCbor,
}: {
  readonly root: string;
  readonly keyBytes: string;
  readonly nonMembershipProofCbor: string;
}): string => {
  const proof = Data.from(nonMembershipProofCbor, Proof);
  const rootData = Data.from(Data.to(root, MerkleRoot));
  const keyData = Data.from(Data.to(keyBytes, asLucidSchema(Data.Bytes())));
  const proofData = Data.from(Data.to(proof, asLucidSchema(Proof)));
  return Data.to(
    [rootData, keyData, proofData],
    asLucidSchema(Data.Array(Data.Any())),
  );
};

export const encodeRawPhasMembershipProofRedeemer = ({
  root,
  keyBytes,
  valueBytes,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly keyBytes: string;
  readonly valueBytes: string;
  readonly membershipProofCbor: string;
}): string => {
  const proof = Data.from(membershipProofCbor, Proof);
  const rootData = Data.from(Data.to(root, MerkleRoot));
  const keyData = Data.from(Data.to(keyBytes, asLucidSchema(Data.Bytes())));
  const valueData = Data.from(Data.to(valueBytes, asLucidSchema(Data.Bytes())));
  const proofData = Data.from(Data.to(proof, asLucidSchema(Proof)));
  return Data.to(
    [rootData, keyData, valueData, proofData],
    asLucidSchema(Data.Array(Data.Any())),
  );
};
