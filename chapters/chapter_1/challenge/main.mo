import Buffer "mo:base/Buffer";
import Text "mo:base/Text";

actor {

    let name : Text = "Halo";

    var manifesto : Text = "we build games";

    let goals = Buffer.Buffer<Text>(1);

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        return Buffer.toArray<Text>(goals);
    };
};