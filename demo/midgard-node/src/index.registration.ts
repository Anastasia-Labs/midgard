import { normalizeHex } from "@al-ft/midgard-core/hex";
import { Command } from "commander";

import packageJson from "../package.json" with { type: "json" };
import * as AvailabilityChallengeCommand from "./commands/availability-challenge.js";
import {
  failCli,
  parseNonNegativeIntegerOption,
  parsePositiveIntegerOption,
  parseStringListOption,
  writeJson,
} from "./commands/cli-runtime.js";
import * as DaBondCommand from "./commands/da-bond.js";
import { type DaLibp2pPreflightMode } from "./da/libp2p-producer.js";
import {
  DA_LIBP2P_RUNTIME_PROFILES,
  type DaLibp2pRuntimeManifestOptions,
  type DaLibp2pRuntimeManifestTarget,
} from "./da/libp2p-runtime-manifest.js";
import { loadRuntimeDotenv } from "./runtime-env.js";
import { chalk, ENV_VARS_GUIDE } from "./utils.js";

loadRuntimeDotenv();

export const VERSION = packageJson.version;

export const program = new Command();

export const parseMerkleRootOption = (
  value: unknown,
  label: string,
): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be 32 bytes of hex`);
  }
  try {
    return normalizeHex(value, { byteLength: 32, trim: false });
  } catch {
    throw new Error(`${label} must be 32 bytes of hex`);
  }
};

export const parseOptionalHeaderHashOption = (
  value: unknown,
): string | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string") {
    throw new Error("--header-hash must be 28 bytes of hex");
  }
  return normalizeHex(value, { byteLength: 28, trim: false });
};

export const parseOptionalEndTimeMs = (value: unknown): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error("--end-time-ms must be a non-negative integer");
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error("--end-time-ms must be a safe non-negative integer");
  }
  return parsed;
};

export const parseDaLibp2pPreflightMode = (
  value: unknown,
): DaLibp2pPreflightMode => {
  if (value === "bind-listen" || value === "dial-only") {
    return value;
  }
  throw new Error("--mode must be bind-listen or dial-only");
};

const DA_LIBP2P_RUNTIME_TARGETS = new Set(["producer", "committee"]);

export const parseDaLibp2pRuntimeTarget = (
  value: unknown,
): DaLibp2pRuntimeManifestTarget => {
  if (typeof value === "string" && DA_LIBP2P_RUNTIME_TARGETS.has(value)) {
    return value as DaLibp2pRuntimeManifestTarget;
  }
  throw new Error("--target must be producer or committee");
};

export const parseDaLibp2pRuntimeProfile = (
  value: unknown,
): DaLibp2pRuntimeManifestOptions["profile"] => {
  if (
    typeof value === "string" &&
    (DA_LIBP2P_RUNTIME_PROFILES as readonly string[]).includes(value)
  ) {
    return value as DaLibp2pRuntimeManifestOptions["profile"];
  }
  throw new Error(
    `--profile must be one of ${DA_LIBP2P_RUNTIME_PROFILES.join(", ")}`,
  );
};

const parseDaLibp2pCommitteeMember = (
  value: string,
): DaLibp2pRuntimeManifestOptions["committeeMembers"][number] => {
  const [signerIndexRaw, daVkey, keySource, rolesRaw, endpointRaw, ...extra] =
    value.split(",");
  if (
    signerIndexRaw === undefined ||
    daVkey === undefined ||
    keySource === undefined ||
    rolesRaw === undefined ||
    extra.length > 0
  ) {
    throw new Error(
      "--committee-member must use signerIndex,daVkey,libp2pKeySource,role+role[,[host:]port]",
    );
  }
  const roles = rolesRaw
    .split("+")
    .map((role) => role.trim())
    .filter((role) => role.length > 0);
  if (roles.length === 0) {
    throw new Error("--committee-member roles must be non-empty");
  }
  return {
    signerIndex: parseNonNegativeIntegerOption(
      signerIndexRaw,
      "--committee-member signerIndex",
    ),
    daVkey,
    libp2pPrivateKeySource: keySource,
    roles,
    ...(endpointRaw === undefined
      ? {}
      : { endpoint: parseDaLibp2pCommitteeMemberEndpoint(endpointRaw) }),
  };
};

const parseDaLibp2pCommitteeMemberEndpoint = (
  value: string,
): { readonly host?: string; readonly port: number } => {
  const separator = value.lastIndexOf(":");
  const host = separator === -1 ? undefined : value.slice(0, separator);
  if (host !== undefined && host.length === 0) {
    throw new Error("--committee-member endpoint host must be non-empty");
  }
  const port = parsePositiveIntegerOption(
    value.slice(separator + 1),
    "--committee-member endpoint port",
  );
  return host === undefined ? { port } : { host, port };
};

export const parseDaLibp2pCommitteeMembers = (
  values: unknown,
): DaLibp2pRuntimeManifestOptions["committeeMembers"] =>
  parseStringListOption(values, "--committee-member").map((value) =>
    parseDaLibp2pCommitteeMember(value),
  );

program.version(VERSION).description(
  `
  ${chalk.red(
    `                       @#
                         @@%#
                        %@@@%#
                       %%%%%%##
                      %%%%%%%%%#
                     %%%%%%%%%%%#
                    %%%%%%%%%%####
                   %%%%%%%%%#######
                  %%%%%%%%  ########
                 %%%%%%%%%  #########
                %%%%%%%%%%  ##########
               %%%%%%%%%%    ##########
              %%%%%%%%%%      ##########
             %%%%%%%%%%        ##########
            %%%%%%%%%%          ##########
           %%%%%%%%%%            ##########
          ###%%%%%%%              ##########
         #########                  #########

   ${chalk.bgGray(
     "    " +
       chalk.bold(
         chalk.whiteBright("A  N  A  S  T  A  S  I  A") +
           "     " +
           chalk.redBright("L  A  B  S"),
       ) +
       "    ",
   )}
  `,
  )}
          ${"Midgard Node – Demo CLI Application"}
  ${ENV_VARS_GUIDE}`,
);

const availabilityChallenge = program
  .command("availability-challenge")
  .description(
    "Operate and recover on-chain DA availability challenges with an isolated actor wallet",
  );

for (const action of [
  "open",
  "respond",
  "settle",
  "close",
  "timeout",
  "status",
  "recover",
] as const) {
  availabilityChallenge
    .command(action)
    .description(
      action === "timeout"
        ? "Advance expired tranche settlement, unavailable timeout and locked descendant removal"
        : `Run availability ${action}`,
    )
    .requiredOption(
      "--manifest <path>",
      "Verified finalized contract deployment manifest",
    )
    .requiredOption(
      "--journal <path>",
      "Absolute durable SQLite journal shared by this actor",
    )
    .requiredOption("--header-hash <hex>", "28-byte header hash")
    .requiredOption(
      "--wallet-seed-env <name>",
      "Explicit environment variable containing the dedicated actor mnemonic",
    )
    .option(
      "--collateral-out-ref <hash#index>",
      "Reserved actor plain ADA collateral",
    )
    .option(
      "--funding-out-ref <hash#index>",
      "Exact challenger opening bond or removal fee funding",
    )
    .option(
      "--payload-file <path>",
      "Exact retained envelope bytes for a response",
    )
    .option(
      "--tranche-index <index>",
      "Respond on a specific active tranche",
      (value: string) =>
        parseNonNegativeIntegerOption(value, "--tranche-index"),
    )
    .option(
      "--kupo-url <url>",
      "Local canonical Kupo URL; defaults to L1_KUPO_KEY",
    )
    .option(
      "--ogmios-url <url>",
      "Local canonical Ogmios URL; defaults to L1_OGMIOS_KEY",
    )
    .action(
      async (
        options: AvailabilityChallengeCommand.AvailabilityCommandOptions,
      ) => {
        try {
          writeJson(
            await AvailabilityChallengeCommand.runAvailabilityChallengeCommand(
              action,
              options,
            ),
          );
        } catch (error) {
          failCli(`availability-challenge ${action}`, error);
        }
      },
    );
}

export const daBond = program
  .command("da-bond")
  .description(
    "Operate the pooled DA committee bond: status, top-up, and owner-quorum withdrawal",
  );

export const daBondChainOptions = (command: Command): Command =>
  command
    .requiredOption(
      "--manifest <path>",
      "Verified finalized contract deployment manifest",
    )
    .option(
      "--kupo-url <url>",
      "Local canonical Kupo URL; defaults to L1_KUPO_KEY",
    )
    .option(
      "--ogmios-url <url>",
      "Local canonical Ogmios URL; defaults to L1_OGMIOS_KEY",
    );

daBondChainOptions(
  daBond
    .command("status")
    .description("Print the pool's state, backing and unlock_at"),
).action(async (options: DaBondCommand.DaBondChainOptions) => {
  try {
    writeJson(await DaBondCommand.runDaBondStatus(options));
  } catch (error) {
    failCli("da-bond status", error);
  }
});
