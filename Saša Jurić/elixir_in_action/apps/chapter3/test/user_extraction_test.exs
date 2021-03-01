defmodule UserExtractionTest do
  use ExUnit.Case
  doctest UserExtraction

  test "extract user with empty map" do
    assert UserExtraction.extract_user(%{}) == {:error, "login missing"}
  end

  test "extract user with valid login and email missing" do
    assert UserExtraction.extract_user(%{"login" => "user"}) ==
             {:error, "email missing"}
  end

  test "extract user with valid login, valid email and missing password" do
    assert UserExtraction.extract_user(%{"login" => "user", "email" => "user@example.com"}) ==
             {:error, "password missing"}
  end

  test "extract user with valid login, valid email and valid password" do
    assert UserExtraction.extract_user(%{
             "login" => "user",
             "email" => "user@example.com",
             "password" => "weak password"
           }) ==
             {:ok, %{login: "user", email: "user@example.com", password: "weak password"}}
  end
end
