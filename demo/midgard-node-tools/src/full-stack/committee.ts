import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { deriveOperatorDaParams } from "midgard-node/transactions/initialization.derive-operator-da-params";

import type { StackConfig } from "./config.js";

/** Use the same sorted signer indexes in initialization and every service. */
export async function configureStackCommittee(
  config: StackConfig,
  env: Record<string, string>,
) {
  const members = config.da.members
    .map((member) => {
      const wallet = walletFromSeed(env[member.seedEnv]!, {
        network: "Preprod",
      });
      const daVkey = Buffer.from(
        CML.PrivateKey.from_bech32(wallet.paymentKey)
          .to_public()
          .to_raw_bytes(),
      ).toString("hex");
      return { ...member, daVkey };
    })
    .sort((left, right) =>
      left.daVkey < right.daVkey ? -1 : left.daVkey > right.daVkey ? 1 : 0,
    );
  if (new Set(members.map((member) => member.daVkey)).size !== members.length)
    throw new Error("DA committee members must have distinct signing keys");
  const committee = members.map((member) => member.daVkey).join("");
  if (env.DA_COMMITTEE_HEX && env.DA_COMMITTEE_HEX !== committee)
    throw new Error("DA members differ from the configured sorted committee");
  const params = await Effect.runPromise(
    deriveOperatorDaParams({
      NETWORK: "Preprod",
      L1_OPERATOR_SEED_PHRASE: env.L1_OPERATOR_SEED_PHRASE!,
      DA_COMMITTEE_HEX: committee,
      DA_THRESHOLD: BigInt(env.DA_THRESHOLD!),
      DA_OWNERS_HEX: env.DA_OWNERS_HEX,
      DA_COSIGNER_SEED_PHRASE: env.DA_COSIGNER_SEED_PHRASE,
    }),
  );
  env.DA_COMMITTEE_HEX = params.committee;
  env.DA_OWNERS_HEX = params.owners.join("");
  return members.map((member, signerIndex) => ({ ...member, signerIndex }));
}
