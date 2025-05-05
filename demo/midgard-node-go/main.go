package main

import (
	"fmt"
	"os"

	"github.com/Anastasia-Labs/midgard-node-go/cmd"
	"github.com/Anastasia-Labs/midgard-node-go/pkg"
	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "midgard",
	Short: "midgard",
	Long:  `midgard`,
	Run:   func(cmd *cobra.Command, args []string) {},
}

func init() {
	pkg.AddConfigFlag(rootCmd)
	rootCmd.AddCommand(cmd.NodeCmd)

}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Whoops. There was an error while executing your CLI '%s'", err)
		os.Exit(1)
	}
}
