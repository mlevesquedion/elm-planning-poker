require "./spec_helper"

describe Kemal::Chat::Elm do
  # TODO: Write tests

  it "works" do
    ws "/chat"
    false.should eq(true)
  end
end
