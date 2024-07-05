import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Types "types";
import Option "mo:base/Option";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Subnauticoin";
    };

    public query func tokenSymbol() : async Text {
        return "_o/";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let money = Option.get(ledger.get(owner), 0);
        ledger.put(owner, amount + money);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let money = Option.get(ledger.get(owner), 0);
        if (money < amount) {
            return #err("Insuficcient amount to burn");
        };
        ledger.put(owner, money - amount);
        return #ok();
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        if (from == to) {
            return #err("Cannot transfer to self");
        };
        let moneyFrom = Option.get(ledger.get(from), 0);
        let moneyTo = Option.get(ledger.get(to), 0);

        if (moneyFrom < amount) {
            return #err("Not enough balance to transfer");
        };

        ledger.put(from, moneyFrom - amount);
        ledger.put(to, moneyTo + amount);

        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        let money = Option.get(ledger.get(account), 0);
        return money;
    };

    public query func totalSupply() : async Nat {
        var total : Nat = 0;
        for (x in ledger.vals()) {
            total := total + x;
        };
        return total;
    };

};