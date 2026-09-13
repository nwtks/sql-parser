module SqlParser.Tests.AccessControlTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``GRANT verification`` () =
    match parse "GRANT SELECT, INSERT, UPDATE (name) ON TABLE users TO alice, bob WITH GRANT OPTION" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select None
                                                                PrivilegeAction.Insert None
                                                                PrivilegeAction.Update(Some [ { Kind = Identifier "NAME" } ]) ],
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "ALICE" }; { Kind = Identifier "BOB" } ],
                                           false,
                                           true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges, got %A" res)

    match parse "GRANT ALL PRIVILEGES ON users TO PUBLIC" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.AllPrivileges,
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "PUBLIC" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges ALL, got %A" res)

    match parse "GRANT SELECT ON users TO alice WITH HIERARCHY OPTION WITH GRANT OPTION GRANTED BY CURRENT_USER" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           true,
                                           true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges WITH HIERARCHY OPTION, got %A" res)

    match parse "GRANT role_a, role_b TO alice WITH ADMIN OPTION" with
    | Grant(GrantStatement.GrantRoles([ { Kind = Identifier "ROLE_A" }; { Kind = Identifier "ROLE_B" } ],
                                      [ { Kind = Identifier "ALICE" } ],
                                      true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantRoles, got %A" res)

[<Fact>]
let ``GRANT UNDER privilege verification`` () =
    match parse "GRANT UNDER ON TABLE users TO alice" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Under ],
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges UNDER, got %A" res)

[<Fact>]
let ``GRANT EXECUTE ON routine verification`` () =
    match parse "GRANT EXECUTE ON FUNCTION add TO alice" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Execute ],
                                           { Kind = Identifier "ADD" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GRANT EXECUTE ON FUNCTION, got %A" res)

    match parse "GRANT SELECT ON PROCEDURE p TO bob" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                           { Kind = Identifier "P" },
                                           [ { Kind = Identifier "BOB" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GRANT SELECT ON PROCEDURE, got %A" res)

    match parse "GRANT SELECT (SPECIFIC FUNCTION f) ON TYPE my_type TO alice" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select(Some(PrivilegeMethods [ designator ])) ],
                                           { Kind = Identifier "MY_TYPE" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) ->
        Assert.True(designator.IsSpecific)
        Assert.Equal(Some RoutineType.Function, designator.RoutineType)
        Assert.Equal(Identifier "F", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected GRANT SELECT (method list), got %A" res)

[<Fact>]
let ``CREATE ROLE verification`` () =
    match parse "CREATE ROLE admin" with
    | CreateRole { Kind = Identifier "ADMIN" } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRole, got %A" res)

    match parse "CREATE ROLE analyst WITH ADMIN CURRENT_USER" with
    | CreateRole { Kind = Identifier "ANALYST" } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRole WITH ADMIN, got %A" res)

[<Fact>]
let ``REVOKE verification`` () =
    match parse "REVOKE SELECT, DELETE ON users FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None; PrivilegeAction.Delete ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ],
                                              NoOption,
                                              true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges, got %A" res)

    match parse "REVOKE GRANT OPTION FOR SELECT ON users FROM alice RESTRICT" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ],
                                              GrantOptionFor,
                                              false)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges GRANT OPTION FOR, got %A" res)

    match parse "REVOKE HIERARCHY OPTION FOR SELECT ON users FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ],
                                              HierarchyOptionFor,
                                              true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges HIERARCHY OPTION FOR, got %A" res)

    match parse "REVOKE role_a FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                                         [ { Kind = Identifier "ALICE" } ],
                                         false,
                                         true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles, got %A" res)

    match parse "REVOKE ADMIN OPTION FOR role_a FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                                         [ { Kind = Identifier "ALICE" } ],
                                         true,
                                         true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles ADMIN OPTION FOR, got %A" res)

[<Fact>]
let ``REVOKE requires drop behavior`` () =
    parseFails "REVOKE SELECT ON users FROM alice"
    parseFails "REVOKE role_a FROM alice"
