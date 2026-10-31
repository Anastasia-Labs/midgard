import { readFileSync } from "node:fs";
import { resolve } from "node:path";

export const EXIT = {
  committed: 0,
  refused: 1,
  usage: 2,
  unchecked: 3,
  resyncFailed: 4,
};

const BLUEPRINT = "onchain/aiken/plutus.json";

export const MAX_BUFFER = 256 * 1024 * 1024;

const ATTRIBUTION = [
  /^\s*co-authored-by:.*\b(?:claude|anthropic|openai|codex|copilot|chatgpt|gpt-?\d|gemini|cursor)\b/imu,
  /\bgenerated (?:with|by)\b.*\b(?:claude|anthropic|openai|codex|copilot|chatgpt|gemini|cursor)\b/imu,
  /noreply@anthropic\.com/iu,
  /\u{1F916}/u,
];

export const USAGE = `usage: commit-paths -m <message> [-m <paragraph>]... | -F <file>
                    [--patch <file>] [--allow-unchecked] [--dry-run] -- <paths...>

Commits exactly <paths> from the working tree through a temporary index.
With --patch, commits exactly the hunks in <file> (a diff against HEAD);
paths, if given, must match the files the patch touches.`;

export class Stop extends Error {
  constructor(code, message) {
    super(message);
    this.code = code;
  }
}

export const refuse = (message) => {
  throw new Stop(EXIT.refused, `refused: ${message}`);
};

export const usage = (message) => {
  throw new Stop(EXIT.usage, `${message}\n\n${USAGE}`);
};

export const parseArgs = (argv) => {
  const options = {
    messages: [],
    messageFile: undefined,
    patch: undefined,
    allowUnchecked: false,
    dryRun: false,
    paths: [],
    help: false,
  };
  const value = (index, flag) => {
    if (index >= argv.length) usage(`${flag} needs a value`);
    return argv[index];
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--") {
      options.paths.push(...argv.slice(index + 1));
      break;
    }
    if (arg === "-m" || arg === "--message") {
      index += 1;
      options.messages.push(value(index, arg));
    } else if (arg === "-F" || arg === "--file") {
      index += 1;
      if (options.messageFile !== undefined) usage("-F given twice");
      options.messageFile = value(index, arg);
    } else if (arg === "--patch") {
      index += 1;
      options.patch = value(index, arg);
    } else if (arg === "--allow-unchecked") {
      options.allowUnchecked = true;
    } else if (arg === "--dry-run") {
      options.dryRun = true;
    } else if (arg === "-h" || arg === "--help") {
      options.help = true;
    } else if (arg.startsWith("-")) {
      usage(`unknown option ${arg}`);
    } else {
      options.paths.push(arg);
    }
  }
  return options;
};

export const gitEnv = (index) => {
  const env = { ...process.env, GIT_LITERAL_PATHSPECS: "1" };
  delete env.GIT_INDEX_FILE;
  if (index !== undefined) env.GIT_INDEX_FILE = index;
  return env;
};

export const nulFields = (buffer) =>
  buffer
    .toString("utf8")
    .split("\0")
    .filter((field) => field.length > 0);

export const refuseBlueprint = (paths) => {
  if (paths.includes(BLUEPRINT)) {
    refuse(
      `${BLUEPRINT} is a build output of whichever compiler and profile last ran; it is never committed`,
    );
  }
};

// --------------------------------------------------------------- message

export const buildMessage = (options, cwd) => {
  if (options.messages.length > 0 && options.messageFile !== undefined) {
    usage("give -m or -F, not both");
  }
  let message;
  if (options.messageFile !== undefined) {
    try {
      message = readFileSync(resolve(cwd, options.messageFile), "utf8");
    } catch (error) {
      usage(`cannot read message file: ${error.message}`);
    }
  } else if (options.messages.length > 0) {
    message = options.messages.join("\n\n");
  } else {
    usage("a commit message is required (-m or -F)");
  }
  message = `${message.replace(/\s+$/u, "")}\n`;
  if (message.trim().length === 0) refuse("the commit message is empty");
  for (const pattern of ATTRIBUTION) {
    if (pattern.test(message)) {
      refuse(
        "the commit message carries an tool attribution line or trailer; commits here carry no tool attribution",
      );
    }
  }
  return message;
};

// ------------------------------------------------------------ formatting

export const isPrettierPath = (path) =>
  path.startsWith("demo/") && /\.(?:ts|tsx|md)$/u.test(path);

export const isAikenPath = (path) => path.endsWith(".ak");
