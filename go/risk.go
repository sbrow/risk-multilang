package risk

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/sbrow/prob/dice"
)

const Delim = ","

// loadRolls loads a roll table from path
func loadRolls(n int) []string {
	d := make([]dice.Die, n)
	for i := 0; i < n; i++ {
		d[i] = dice.D6()
	}
	rolls, err := dice.NewTable(d)
	if err != nil {
		panic(err) // TODO(sbrow): Fix
	}
	var out []string
	for _, s := range rolls.Data {
		out = append(out, s[0])
	}
	return out
}

func sortRoll(s string) string {
	i := strToInt(s)
	sort.Sort(sort.Reverse(sort.IntSlice(i)))
	s = fmt.Sprint(i[0], s[1:])
	for j := 2; j <= len(s)-1; j += 2 {
		s = fmt.Sprint(s[:j], i[j/2], s[j+1:])
	}
	s = fmt.Sprint(s[:len(s)-1], i[len(i)-1])
	return s
}

// Compare enumerates the results of a combat.
//
// TODO(sbrow): Rename
func Compare(atk, def int, mods ...string) (win, tie, loss float64) {
	if atk < 1 || def < 1 {
		return
	}
	sum := 0.0
	for _, atkRoll := range loadRolls(min(atk, 3)) {
		atkRoll = sortRoll(atkRoll)
		for _, defRoll := range loadRolls(min(def, 2)) {
			defRoll = sortRoll(defRoll)
			lost := Combat(atkRoll, defRoll, mods...)
			switch {
			case lost == 0:
				win++
			case lost == 1:
				if atk > 1 && def > 1 {
					tie++
				} else {
					loss++
				}
			case lost == 2:
				loss++
			}
			sum++
		}
	}

	return win / sum, tie / sum, loss / sum
}

// strToInt converts a string value to a list of ints,
// using rolltable.Delim as the delimiter.
func strToInt(s string) []int {
	i := []int{}
	for _, t := range strings.Split(s, dice.Delim) {
		n, err := strconv.Atoi(t)
		if err != nil {
			panic(err)
		}
		i = append(i, n)
	}
	return i
}

// Combat determines the winner of a dice clash.
func Combat(atk, def string, mod ...string) int {
	a, d := strToInt(atk), strToInt(def)
	addModifiers(&a, &d, mod...)

	short := min(len(a), len(d))

	lost := 0
	for i := 0; i < short; i++ {
		if d[i] >= a[i] {
			lost++
		}
	}
	return lost
}

// addModifiers adds modifiers to the dice roll
func addModifiers(atk, def *[]int, mod ...string) {
	sort.Ints(*atk)
	sort.Ints(*def)
	if len(*atk) > len(*def) {
		*atk = (*atk)[len(*def)-1:]
	}
	for _, m := range mod {
		switch m {
		case "fortification":
			if len(*def) == 2 {
				(*def)[1] = min((*def)[1]+1, 6)
			}
			fallthrough
		case "bunker":
			(*def)[0] = min((*def)[0]+1, 6)
		case "ammo shortage":
			i := 0
			if len(*def) == 2 {
				i = 1
			}
			(*def)[i] = max((*def)[1]-1, 0)
		}
	}
	sort.Sort(sort.Reverse(sort.IntSlice(*atk)))
	sort.Sort(sort.Reverse(sort.IntSlice(*def)))
}
