// Command l1-mock-node serves the deterministic mock N2C peer on a unix
// socket for the TypeScript conformance tests. It reads one JSON command per
// stdin line and answers each with one JSON line on stdout. It is a test
// tool and never part of a deployment.
package main

import (
	"bufio"
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"time"

	"github.com/anastasia-labs/midgard-l1-node-transport/mocknode"
)

type command struct {
	Op     string  `json:"op"`
	Count  int     `json:"count"`
	Branch uint64  `json:"branch"`
	Height uint64  `json:"height"`
	Tag    uint64  `json:"tag"`
	Raw    *string `json:"raw"`
	Ms     int64   `json:"ms"`
}

type wireBlock struct {
	Number uint64 `json:"number"`
	Slot   uint64 `json:"slot"`
	Hash   string `json:"hash"`
	Prev   string `json:"prev"`
	Raw    string `json:"raw"`
}

func blocks(in []mocknode.Block) []wireBlock {
	out := make([]wireBlock, 0, len(in))
	for _, b := range in {
		out = append(out, wireBlock{Number: b.Number, Slot: b.Slot, Hash: hex.EncodeToString(b.Hash), Prev: hex.EncodeToString(b.Prev), Raw: hex.EncodeToString(b.Raw)})
	}
	return out
}

func main() {
	socket := flag.String("socket", "", "unix socket path")
	magic := flag.Uint("magic", 42, "network magic")
	flag.Parse()
	if *socket == "" {
		fmt.Fprintln(os.Stderr, "--socket is required")
		os.Exit(64)
	}
	node, err := mocknode.Start(*socket, uint32(*magic))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(69)
	}
	defer node.Close()
	out := json.NewEncoder(os.Stdout)
	_ = out.Encode(map[string]any{"ready": true})
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Buffer(make([]byte, 1<<20), 1<<24)
	for scanner.Scan() {
		var c command
		if err := json.Unmarshal(scanner.Bytes(), &c); err != nil {
			_ = out.Encode(map[string]any{"error": err.Error()})
			continue
		}
		reply := map[string]any{"ok": true}
		raw := func() []byte {
			if c.Raw == nil {
				return nil
			}
			decoded, err := hex.DecodeString(*c.Raw)
			if err != nil {
				reply = map[string]any{"error": err.Error()}
			}
			return decoded
		}
		switch c.Op {
		case "extend":
			added, err := node.Extend(c.Count, c.Branch)
			if err != nil {
				reply = map[string]any{"error": err.Error()}
				break
			}
			reply["blocks"] = blocks(added)
		case "rollback":
			node.Rollback(c.Height)
		case "chain":
			reply["blocks"] = blocks(node.Chain())
		case "reject":
			node.SetRejectReason(raw())
		case "answer":
			node.SetShelleyAnswer(c.Tag, raw())
		case "drop":
			node.DropConnections()
		case "ledgerDelay":
			node.SetLedgerDelay(time.Duration(c.Ms) * time.Millisecond)
		case "clearMempool":
			node.ClearMempool()
		case "stats":
			reply["requestNexts"] = node.RequestNexts.Load()
			reply["connections"] = node.Connections.Load()
			reply["mempool"] = len(node.Mempool())
			reply["acquired"] = node.Acquired()
			reply["hasTxEras"] = node.HasTxEras()
		case "sampleTx":
			tx, id, err := mocknode.SampleTx()
			if err != nil {
				reply = map[string]any{"error": err.Error()}
				break
			}
			reply["tx"] = hex.EncodeToString(tx)
			reply["id"] = hex.EncodeToString(id)
		default:
			reply = map[string]any{"error": "unknown op " + c.Op}
		}
		_ = out.Encode(reply)
	}
}
