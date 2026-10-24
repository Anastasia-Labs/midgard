// fixture-path: midgard-node/src/index.registration.ts
declare const program: { command(name: string): void };

// ruleid: midgard/node-cli-operator-commands-only
program.command("stress-wallets");

// ok: midgard/node-cli-operator-commands-only
program.command("start");
