// fixture-path: midgard-node/src/index.ts
declare const program: any;
declare const suite: string;
declare const Command: new (name: string) => unknown;

// ruleid: midgard/node-cli-operator-commands-only
program.command("bench-throughput");

// ruleid: midgard/node-cli-operator-commands-only
program.command("e2e:deposit <amount>");

// ruleid: midgard/node-cli-operator-commands-only
program.addCommand(new Command("demo"));

// ruleid: midgard/node-cli-operator-commands-only
program.command(`test-${suite}`);

// ok: midgard/node-cli-operator-commands-only
program.command("listen");

// ok: midgard/node-cli-operator-commands-only
program.command("latest-commitment <hash>");
