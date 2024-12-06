program test
  use StringiFor
  use PENF
  implicit none
  character, dimension(*), parameter :: cards = &
    & ['A','K','Q','J','T','9','8','7','6','5','4','3','2']
  logical :: bool

  bool = higher(string('77788'), string('77888'))
  print *, bool
contains
  function count_cards(hand)
    type(string), intent(in) :: hand
    integer, dimension(size(cards)) :: count_cards

    integer :: j, k

    count_cards = 0
    do j = 1, hand%len()
      do k = 1, size(cards)
        if (hand%raw(j:j) == cards(k)) then
          count_cards(k) = count_cards(k) + 1
        end if
      end do
    end do
  end function count_cards

  function higher(hand1, hand2)
    ! Test whether hand2 is higher than hand1
    logical :: higher
    type(string), intent(in) :: hand1, hand2

    integer, dimension(size(cards), 2) :: counts
    integer :: j, types(2)

    integer, parameter :: high_card = 0, one_pair = 1, two_pairs = 2, &
      three_of_a_kind = 3, full_house = 4, four_of_a_kind = 5, &
      five_of_a_kind = 6

    counts(:, 1) = count_cards(hand1)
    counts(:, 2) = count_cards(hand2)

    do j = 1, 2
      if (maxval(counts(:, j)) == 1) then
        types(j) = high_card
        print *, 'High Card'
      else if (maxval(counts(:, j)) == 2) then
        if (count(counts(:,j) == 2) == 1) then
          types(j) = one_pair
          print *, 'One Pair'
        else
          types(j) = two_pairs
          print *, 'Two Pairs'
        end if
      else if (maxval(counts(:, j)) == 3) then
        if (maxval(counts(:, j), mask=(counts(:,j) < maxval(counts(:, j)))) == 2) then
          types(j) = full_house
          print *, 'Full House'
        else
          types(j) = three_of_a_kind
          print *, 'Three of a kind'
        end if
      else if (maxval(counts(:, j)) == 4) then
        types(j) = four_of_a_kind
        print *, 'Four of a kind'
      else
        types(j) = five_of_a_kind
        print *, 'Five of a kind'
      end if
    end do

    if (types(1) /= types(2)) then
      higher = types(2) > types(1)
    else
      card_loop: do j = 1, hand1%len()
        if (hand1%raw(j:j) /= hand2%raw(j:j)) then
          higher = higher_card(hand1%raw(j:j), hand2%raw(j:j))
          exit card_loop
        end if
      end do card_loop
    end if
  end function higher

  function higher_card(card1, card2)
    logical :: higher_card
    character, intent(in) :: card1, card2

    integer, dimension(size(cards)) :: count1, count2

    count1 = count_cards(string(card1))
    count2 = count_cards(string(card2))
    higher_card = all(maxloc(count2) < maxloc(count1))
  end function higher_card
end program test
