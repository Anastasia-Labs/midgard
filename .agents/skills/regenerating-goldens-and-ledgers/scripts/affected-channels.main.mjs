import {
  changedFiles,
  normalizeFile,
  unclaimed,
} from "./affected-channels.channel-triggers.mjs";
import {
  CouldNotLook,
  createTree,
  EXIT_COULD_NOT_LOOK,
  EXIT_OK,
  EXIT_PROBLEMS,
  loadTable,
  parseArguments,
  usage,
} from "./affected-channels.parse-arguments.mjs";
import { affectedChannels } from "./affected-channels.verify-command.mjs";
import {
  renderMapping,
  verifyTable,
} from "./affected-channels.verify-table.mjs";

export const main = (argv, io = { out: console.log, err: console.error }) => {
  let options;
  try {
    options = parseArguments(argv);
    if (options.help) {
      io.out(usage);
      return EXIT_OK;
    }
    const table = loadTable(options.table);
    const tree = createTree(options.root);
    if (options.verifyTable) {
      const problems = verifyTable(tree, table);
      if (options.json)
        io.out(
          JSON.stringify(
            { status: problems.length ? "problems" : "ok", problems },
            null,
            2,
          ),
        );
      else if (problems.length)
        for (const message of problems) io.out(`PROBLEM: ${message}`);
      else
        io.out(
          `verify-table: ok, ${table.channels.length} channels cover every generator, ledger and generated file.`,
        );
      return problems.length ? EXIT_PROBLEMS : EXIT_OK;
    }
    let files;
    let source;
    if (options.files) {
      files = [
        ...new Set(
          options.files.map((file) => normalizeFile(options.root, file)),
        ),
      ].sort();
      source = "(from --files)";
    } else {
      const changed = changedFiles(options.root, options.base);
      files = changed.files;
      source = `(working tree against ${changed.mergeBase.slice(0, 12)}, merge-base of ${options.base} and HEAD, plus untracked)`;
    }
    const affected = affectedChannels(tree, table, files);
    const problems = unclaimed(tree, table, files);
    if (options.json) {
      io.out(
        JSON.stringify(
          {
            status: problems.length
              ? "problems"
              : affected.length
                ? "affected"
                : "none-affected",
            changedFiles: files,
            problems,
            channels: affected.map(({ channel, reasons }) => ({
              id: channel.id,
              kind: channel.kind,
              reasons,
              check: channel.check,
              sync: channel.sync,
              ci: channel.ci,
            })),
          },
          null,
          2,
        ),
      );
    } else io.out(renderMapping({ source, files, affected, problems }));
    return problems.length ? EXIT_PROBLEMS : EXIT_OK;
  } catch (error) {
    if (error instanceof CouldNotLook) {
      if (options?.json)
        io.out(
          JSON.stringify(
            { status: "could-not-look", error: error.message },
            null,
            2,
          ),
        );
      io.err(`could not look: ${error.message}`);
      return EXIT_COULD_NOT_LOOK;
    }
    throw error;
  }
};
